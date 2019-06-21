;;; -*- ;lexical-binding: t; -*-
;;;
;;;      __          __  __
;;;     / /   ____ _/ /_/ /____
;;;    / /   / __ `/ __/ __/ _ \
;;;   / /___/ /_/ / /_/ /_/  __/
;;;  /_____/\__,_/\__/\__/\___/
;;;
;;;
;;; Summary:
;;; Latte - A simple notebook manager with auto highlighting built on top of the
;;; beloved Org-mode.
;;;
;;; Author: Dr. Yad Tahir <yad (at) ieee.org>
;;; Keywords: note-taking, auto-highlighting, org-mode.
;;;
;;; Commentary:
;;; This file is part of a simple note-taking system. Notes are stored in Org
;;; files. This system scans these files on a regular basis to collect
;;; 'keywords'. Latte, then, automatically highlights keywords when the
;;; latte-mode is active. This implementation is designed to be snappy, and
;;; performs UI drawing as less as possible.

;;; License:
;;; Copyright (C) 2019
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301, USA.

;;; Road Map and Hopes:
;;; - Remove the needs for s.el.
;;; - Add support for Helm
;;; - Add Wikipedia integration; it would be cool if the user hover over the
;;; keyword, notebook.el shows the wikipedia definition as a tool tip.
;;;

;; Load cl
(eval-and-compile
  (unless (fboundp 'cl)
	(require 'cl)))

;; Load s.el
(eval-and-compile
  (unless (fboundp 's)
	(require 's)))

;;; Code:
(defgroup latte nil "A simple notebook manager with auto highlighting built on
top of the beloved Org-mode." :group 'latte)

(defcustom latte-directory user-emacs-directory
  "Directory in which note files are stored."
  :group 'latte
  :type 'directory)

(defcustom latte-highlight-prog-comments t
  "If enabled (t), highlight keywords in prog-mode comment sections only."
  :group 'latte
  :type 'boolean)

(defcustom latte-skip-tag "skip"
  "Applying this tag to an Org header makes latte skip processing it."
  :group 'latte
  :type 'string)

(defcustom latte-ignore-words '()
  "The words in the list will not be treated as keywords"
  :group 'latte)

(defcustom latte-scan-idle-delay 10
  "Number of seconds of idle time before re-scanning note files. If this
  variable is set to 0; no idle time is taken.

Changing the value does not take effect until 'latte-mode' has been disabled
for all buffers."
  :group 'latte
  :type 'number)


(defcustom latte-rehighlight-after-scan t
  "If it is not nil, Latte performs UI re-drawing after every scan.

Setting this variable to nil avoids Latte deleting all the existing overlays.
Thus, making the scanning process less drawing intensive. However, the
primary trade off is syncing inconsistency between overlays and backend
keywords. For example, when a keyword is no longer exists, its overlays is not
removed automatically from buffer. A manual refresh is needed then."

  :group 'latte
  :type 'boolean)

(defvar latte-keyword-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-<return>") 'latte-grep-topic)
	;; Add click support
	(define-key map (kbd "<mouse-1>") 'latte-grep-topic)
	(define-key map (kbd "M-<return>") 'latte-grep-all)
	map)

  "Keymap for highlighted keywords.")

(defface latte-keyword-face
  '((t :inherit 'font-lock-keyword-face))
  "Latte mode face used to highlight keywords and topic titles"
  :group 'latte)


;;; Internal variables
(defvar latte--keywords (make-hash-table :test 'equal)
  "Holds list of keywords.

This global data structure is modified primarily by 'latte--scan-keywords'. Both
'latte--highlight' and 'latte--delete-overlays' use this list to update UI
accordingly.")

(defvar latte--history nil
  "A history data type used when notebook.el calls Ivy.")

(defvar latte--keywords-lock nil
  "A lock flag when t, it means there is ongoing modification operations on
  'latte--keywords'. This locker makes latte--keywords resilent to
  concurrency.")

(defvar latte--initialized nil
  "Holds t if notebook's timers are initialized and started. Otherwise, nil.

This variable is used to ensure only one instance of the timers exists
globally.")

(defconst latte--process-name "*latte-keyword-scanner*"
  "Holds the name of the process launched by 'latte--scan-keywords'.")

(defvar-local latte--async-line ""
  "Local variable is used during keyword scanning in 'latte--async-filter'.")


;;;
;;; Helpers
;;;

(defun latte--change-major-mode ()
  "Called internally when the major mode has changed in the current buffer."

  ;; Kill the scanning process if it exists to avoid creating new overlays.
  (latte--kill-processes)
  ;; Delete all old overlays to ensure that the 'latte-highlight-prog-comments'
  ;; setting works.
  (latte--delete-overlays t))

(defun latte--delete-overlays (&optional force)
  "Called internally to delete overlays that are no longer needed.

When FORCE is nil, this function goes through each overlay existing in the
current buffer, and checks whether its underline text is still a keyword. If
it is not, this function deletes the overlay.

However, setting FORCE to t makes this function to delete all overlays without
checking."

  (ignore-errors
	(let ((latte--overlays
		   (append (car (overlay-lists))
				   (cdr (overlay-lists)))))
	  ;; For each overlay
	  (while (not(null latte--overlays))
		(let* ((o (car latte--overlays))
			   (min (overlay-start o))
			   (end (overlay-end o))
			   (f (overlay-get o 'face))
			   (keyword (buffer-substring-no-properties min end)))
		  ;; Make sure it belongs to latte.el.
		  (when (or force
					(and (equal f 'latte-keyword-face)
						 ;; Check if it still points at a keyword
						 (not (member-ignore-case
							   keyword latte--keywords))))
			;; If not
			(delete-overlay o))

		  (setq latte--overlays (cdr latte--overlays)))))))

(defun latte--overlay-exists (keyword start end)
  "Return t if an overlay for KEYWORD exists between START and END."
  (loop for co in (overlays-in start end)
		do
		(progn
		  (when (equal keyword (overlay-get co 'latte-keyword))
			(if (and (equal (overlay-start co) start)
					 (equal (overlay-end co) end))
				(return t)

			  ;; Region mismatch; e.g. an old overlay that does not
			  ;; accommodate the extra length. Clean it and continue searching.
			  (delete-overlay co)) ))))

(defun latte--phrase-checker (phrase)
  "Checks whether PHRASE exists in latte--keywords

If it does not, this function returns nil. Otherwise, PHRASE is returned."

  (when (gethash phrase latte--keywords)
	phrase))

(defun latte--highlight (&optional start end)
  "Highlights all the instances of KEYWORD in the current buffer. For each
  instance, this function creates a clickable overlay.

The optional second argument START indicates starting position. Highlighting
must not occur before START. A value of nil means search from '(point-min)'.

The optional third argument END indicates ending position. Highlight must not
occur after END. A value of nil means search from '(point-max)'."

  ;; Default values
  (setq start (or start (point-min)))
  (setq end (or end (point-max)))

  (save-restriction
	(narrow-to-region start end)
	(save-excursion
	  (with-silent-modifications
		(goto-char (point-min))

		(let ((w nil)
			  (old-w nil)
			  (older-w nil)
			  (pure-w nil))
		  ;; For each word
		  (forward-word)
		  (while (< (point) (point-max))
			;; We can't use text property face as it is overruled by font-lock
			;; highlighting. To solve this problem we have two solutions:
			;;
			;; Solution 1: As shown below, fool lock-face into thinking that the
			;; text is already font-lock highlighted. However, this is not as
			;; consistent as overlays.
			;; (put-text-property (- (point) l)
			;;					 (point)
			;;					 'font-lock-face
			;;					 'latte-keyword-face)
			;; (font-lock-flush (- (point) l)
			;;					 (point))
			;;
			;; Solution 2: Use overlays.
			;; The second solution has been selected since keywords have
			;; to be regularly updated, and a keyword can be part of
			;; other phrases. The last is supported by having multiple
			;; overlays on top of the keyword. The overlay with the
			;; highest priority overshadows the reset. This also
			;; simplifies the needs to re-highlight the nested keywords
			;; in case the phrase is destroyed.

			;; When latte-highlight-prog-comments is on,
			;; overlays in prog-mode must be inside comment
			;; sections only
			(unless (and latte-highlight-prog-comments
						 (derived-mode-p 'prog-mode)
						 ;; Comment section?
						 ;; from https://github.com/blorbx/evil-quickscope
						 (not (nth 4 (syntax-ppss))))

			  (let* ((meta-end 0)
					 (found nil))

				(setq older-w old-w
					  old-w w
					  w (s-downcase(format "%s" (word-at-point))))

				;; Search for a keyword, which can be either:
				;; - A phrase that consists of two or three words.
				;; - A single word (may be even in a plural form)
				(setq pure-w (or (latte--phrase-checker
								  (concat older-w " " old-w " " w))
								 (latte--phrase-checker (concat old-w " " w))
								 (latte--phrase-checker (s-chop-suffix "es" w))
								 (latte--phrase-checker (s-chop-suffix "s" w))
								 (latte--phrase-checker w))
					  found pure-w)

				;; If we are chopping chars
				(when (and found
						   (< (length found) (length w)))
				  ;; Use the word found the the buffer to avoid highlighting
				  ;; less characters
				  (setq found w))

				(when found
				  (let* ((keyword found)
						 (l (length keyword))
						 (beginning (- (point) l))
						 (end  (point)))

					(unless (latte--overlay-exists pure-w beginning end)
					  (let ((o (make-overlay beginning end)))
						(overlay-put o 'face 'latte-keyword-face)
						;; On text modification under the overlay
						(overlay-put o
									 'modification-hooks
									 '((lambda (overlay &rest args)
										 ;; delete the overlay.
										 ;; Re-drawing will occur later
										 ;; if the new text is a member
										 ;; of 'latte--keywords'
										 (delete-overlay overlay))))

						(overlay-put o 'keymap latte-keyword-map)
						(overlay-put o 'mouse-face 'highlight)
						;; The word under POINT may not exist in
						;; latte--keywords. Use the founded phrase instead.
						(overlay-put o 'latte-keyword pure-w)

						;; The priority is calculated based on the number of the
						;; characters. Thus, the overlays of longer phrases are
						;; on top.
						(overlay-put o 'priority l)))))))
			(forward-word)))))))

(defun latte--highlight-buffer ()
  "Check the current buffer and highlight all the keywords."

  (latte--highlight))

(defun latte--add-keyword (keyword)
  "Called internally to add KEYWORD to 'latte--keywords'.

   This functions makes sure that there is no duplicated
   keywords in latte--keywords."

  ;; If it is a new keyword and not black listed!
  (unless (or (gethash keyword latte--keywords)
			  (member keyword latte-ignore-words))

	(puthash keyword keyword latte--keywords)
	;; It is a good idea replace '-' with space and add it as another keyword.
	(when (s-contains? "-" keyword)
	  (let ((k (s-replace "-" " " keyword)))
		(puthash k k latte--keywords)))

	;; Similarly, replace '_'.
	(when (s-contains? "_" keyword)
	  (let ((k (s-replace "_" " " keyword)))
		(puthash k k latte--keywords)
		;; Needed to address 'part1-part2' adjectives.
		(setq k (s-replace "_" "-" keyword))
		(puthash k k latte--keywords)))))

(defun latte--kill-processes ()
  "Terminate the process launched by 'latte--keywords-check'."

  (let ((proc (get-process  latte--process-name)))
	(when proc
	  (delete-process proc))))

(defun latte--async-filter (proc str)
  "A filter function of PROC, the notebook process spawned by
  'latte--scan-keywords'. This method is called automatically when Emacs
  receives outputs from PROC.

Emacs streams the output by calling this method multiple times. STR holds the
output of each batch.

The main objective of this function is to process the output and finds the
keywords."

  ;; Go through each line, which has the format:
  ;; <title> :<tag1>:<tag2>:...:
  (loop for l in (s-lines str)
		do
		(progn
		  ;; The line can be received partially. If this case, save it and
		  ;; concatenate it with the remaining.

		  (if (or (not (s-ends-with? ":" l))
				  (s-blank? l))
			  ;; This line is not complete. Save it temporary and wait for the
			  ;; remaining.
			  (setq latte--async-line
					(concat latte--async-line l))

			;; When line is complete
			(progn
			  ;; Concatenate the remaining if any
			  (setq latte--async-line (concat latte--async-line l))

			  ;; Ignore lines with the skip tag
			  (when (s-contains? latte-skip-tag latte--async-line)
				(setq latte--async-line ""))

			  (setq latte--async-line
					(s-replace "\n" "" latte--async-line))

			  ;; Parse
			  (loop for s in (s-split ":"  latte--async-line)
					do (progn
						 ;; Remove meta characters
						 (setq s (s-trim (s-replace "*" "" s)))
						 (unless (s-blank? s)
						   (latte--add-keyword (s-downcase s))))

					;; Reset the remaining before processing next line
					(setq latte--async-line "")))))))

(defun latte--async-sentinel (process str)
  "Sentinel function of PROCESS spawned by 'latte--scan-keywords'. This
  function is called automatically when the process is finished or interrupted.
  STR indicates the status of the process.

This function triggers UI updates in case 'latte--keywords' has been changed."

  (when (equal 0 (process-exit-status process))
	(when (s-contains? "finished" str)
	  ;; Unlock latte--keywords
	  (setq latte--keywords-lock nil)
	  ;; Check if latte-mode is active on the current buffer
	  (when latte-mode
		(when latte-rehighlight-after-scan
		  ;; The notebook can be updated from another buffer or even
		  ;; externally. There is a possibility that a keyword
		  ;; is removed. To address this case, all overlays of this
		  ;; buffer must be reconstructed.
		  (latte--delete-overlays))

		(latte--highlight-buffer)))))

(defun latte--scan-keywords ()
  "Scan note files and update 'latte--keywords' asynchronously.

This function launches an shell process to go through the note files in the
 directory latte--directory. The sentinel and filter functions is handled by
 latte--async-sentinel and latte--async-filter, respectively."

  (ignore-errors
	;; If the process is still running, kill it
	(latte--kill-processes)
	;; Lock latte--keywords
	(setq latte--keywords-lock t
		  ;; Reset and reconstruct
		  latte--keywords (make-hash-table :test 'equal))

	(let ((c (concat "ag --nocolor --nogroup --only-matching -s %s "
					 latte-directory
					 " --nofilename -G \".org$\"")))
	  (let ((proc (start-process-shell-command
				   latte--process-name
				   (get-buffer-create "*Messages*")
				   (format c (shell-quote-argument "^[\\*].+[:].+[:]$")))))
		(set-process-filter proc 'latte--async-filter)
		(set-process-sentinel proc 'latte--async-sentinel)))))

(defun latte--keyword-at-point()
  "Return the highlighted keyword at point."

  (let ((p (overlays-at (point) t)))
	(if p
		(progn (when (listp p)
				 (setq p (car p)))
			   (downcase (overlay-get p 'latte-keyword)))
	  (word-at-point))))

(defun latte--after-change-function (beginning end &optional old-len)
  "Highlight new keywords text modification events occur."

  ;; The different between BIGINNING and END can be as small as one character.
  (when (< (- end beginning) 3)
	;; Scan the hole line instead
	(setq beginning (line-beginning-position)
		  end (line-end-position)))
  (latte--highlight beginning end))

(defun latte--keywords-taggable ()
  "Go through 'latte--keyword' to generate a list of keywords usable as Org
tags. Spaces and '-' are replaced by '_'."

  (delete-dups
   (append (loop for k in (hash-table-keys latte--keywords)
				 collect
				 (progn
				   ;; Space is not allowed in Org tags
				   (when (s-contains? " " k)
					 (setq k (s-replace " " "_" k)))
				   ;; '-' is also not allowed!
				   (when (s-contains? "-" k)
					 (setq k (s-replace "-" "_" k)))
				   ;; Return the processed keyword
				   k))
		   ;; Push the skip tag in case it is needed.
		   (list latte-skip-tag))))

(defun latte--insert-org-tag-handler (keyword)
  "Insert KEYWORD as an Org tag. This function is called automatically by Ivy,
spawned in 'latte-insert-org-tag'."

  (when (listp keyword)
	(setq keyword (cdr keyword)))

  (let ((tags (org-get-tags)))
	(if (and (s-blank? (car tags))
			 (equal (length tags) 1))
		(progn
		  (org-set-tags-to keyword)
		  (org-set-tags nil t))
	  (unless (member keyword tags)
		(push keyword (cdr (last tags)))
		(org-set-tags-to tags)
		;; Re-align the tags
		(org-set-tags nil t)))))


(defun latte--insert-keyword-handler (keyword)
  "Insert KEYWORD at point. Point and after-insertion markers move forward to
  end up after the inserted text. This function is called automatically by Ivy,
  spawned in 'latte-insert-keyword'."

  (when (listp keyword)
	(setq keyword (cdr keyword)))
  (insert keyword))

;;;
;;; Minor mode
;;;

;;;###autoload
(define-minor-mode latte-mode
  "Minor mode highlights notebook's keywords throughout the buffer.

Initially, highlighting takes place after 'latte-scan-idle-delay'."
  nil
  :lighter latte
  :keymap nil
  :require 'latte
  :group 'latte

  (if latte-mode
	  ;; on
	  (progn
		;; Add local hooks
		(add-hook 'after-change-functions 'latte--after-change-function t t)
		(add-hook 'after-revert-hook 'latte--highlight-buffer t t)
		(add-hook 'after-save-hook 'latte--highlight-buffer t t)
		(add-hook 'change-major-mode-hook 'latte--change-major-mode nil t)
		(add-hook 'edit-server-done-hook 'latte--highlight-buffer t t)

		(latte--highlight-buffer)
		;; Check if the global timer has started
		(unless latte--initialized
		  (latte--scan-keywords)
		  (run-with-idle-timer latte-scan-idle-delay t
							   #'latte--scan-keywords)
		  (setq latte--initialized t)))

	;; off
	(progn
	  ;; Un-highlight
	  (latte--delete-overlays t)

	  ;; Remove local hooks
	  (remove-hook 'after-change-functions 'latte--after-change-function t)
	  (remove-hook 'after-revert-hook 'latte--highlight-buffer t)
	  (remove-hook 'after-save-hook 'latte--highlight-buffer t)
	  (remove-hook 'change-major-mode-hook 'latte--change-major-mode)
	  (remove-hook 'edit-server-done-hook 'latte--highlight-buffer t))))

;;;###autoload
(defun latte-new-entry ()
  "Create a note and store it in 'latte-directory'.

This function prompts the user to select topic name. Then, it auto generates a
simple Org file for the given topic."

  (interactive)
  ;; @FIX: the code here can be improved
  (let ((name (read-string "Topic Name: ")))
	(let ((file-path
		   (concat latte-directory "/"
				   (downcase name) "-"
				   (format-time-string "%s") ".org")))
	  (find-file file-path)
	  (insert (concat
			   "#+ARCHIVE: ~/notes/notebook/archive-notes.org::\n\n"
			   "* " name ))
	  (unless (fboundp 'org)
		(require 'org))
	  (latte-insert-org-tag)
	  (goto-char (point-max))
	  (insert (concat
			   "\n  :PROPERTIES:\n"
			   "  :END:\n  "))
	  ;; Switch temporarily to avoid having problems with org agenda
	  (text-mode)
	  (save-buffer)
	  ;; Reload major mode
	  (revert-buffer t t))))

;;;###autoload
(defun latte-search (&optional init-input)
  "Interactively search through the notes. INIT-INPUT can be passed as the
  initial search query."

  (interactive)
  ;; Pass a regex to ask ag to discard org metadata.
  ;;^[] beginning of the line
  ;;[^] not
  ;; * zero or more char
  ;;(counsel-ag "^[^#]\|[ ]*[^:] " "~/notes" "--nomultiline" )
  (unless (fboundp 'counsel)
	(require 'counsel))
  (setq init-input (or init-input "^[*]+.*"))
  (counsel-ag init-input latte-directory "--nomultiline -G \".org$\""
			  "Search "))

;;;###autoload
(defun latte-files ()
  "Gets list of note files along with their headers."
  (interactive)
  (latte-search "^[\\*][[:space:]]"))

;;;###autoload
(defun latte-grep-topic ()
  "Search for the keyword at point in the notebook, and then show all the
  headings in which the keyword has been used."

  (interactive)
  (latte-search
   (concat "^[*]+[[:space:]].*"
		   (s-replace " " "." (latte--keyword-at-point)) ".*[:]$")))

;;;###autoload
(defun latte-grep-all ()
  "Search for the keyword at point in the notebook, and then show all the text
paragraphs in which the keyword has been used."

  (interactive)
  (latte-search
   (s-replace " " "." (latte--keyword-at-point))))

;;;###autoload
(defun latte-insert-keyword ()
  "Prompt the user to select a keyword, then insert at point."

  (interactive)
  (unless (fboundp 'ivy)
	(require 'ivy))
  (ivy-read "Select a keyword: "
			(hash-table-keys latte--keywords)
			:require-match nil
			:history 'latte--history
			:action 'latte--insert-keyword-handler))

;;;###autoload
(defun latte-insert-org-tag ()
  "Prompt the user to select a keyword, then insert it as an Org tag."

  (interactive)
  (unless (fboundp 'ivy)
	(require 'ivy))

  (unless (fboundp 'org)
	(require 'org))

  (ivy-read "Select a keyword: "
			(latte--keywords-taggable)
			:require-match nil
			:history 'latte--history
			:action 'latte--insert-org-tag-handler))

(provide 'latte)

;;; latte.el ends here
