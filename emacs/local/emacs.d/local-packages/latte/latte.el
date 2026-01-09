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
;;; Copyright (C) 2026
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
;;; - Make Latte less aggressive towards finding plural nouns.
;;; - Remove the needs for s.el.
;;; - Add support for Helm.
;;;

(eval-and-compile
  (unless (fboundp 'cl-lib)
	(require 'cl-lib))
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

Changing the value does not take effect until next Emacs reboot."
  :group 'latte
  :type 'number)

(defcustom  latte-predict-other-forms t
  "If t, Latte guesses other forms associated with a keyword.

Setting this option allows Latte to add other possible word forms, such as
predicting singular and/or plural forms. For instance, Latte adds the words
'table' and 'symbol' when it finds the keywords 'tables' and 'symbols',
respectively."

  :group 'latte
  :type 'boolean)

(defvar latte-keyword-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<mouse-1>") 'latte-grep-topic)
	(define-key map (kbd "<M-return>") 'latte-grep-topic)
	(define-key map (kbd "<M-S-return>") 'latte-grep-all)
	map)

  "Keymap for highlighted keywords.")

(defvar-local latte--prev-start-win 0
  "Holds window start position before scrolling.")

(defvar-local latte--prev-end-win 0
  "Holds window end position before scrolling.")

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

(defconst latte--text-change-line-margin 10
  "This threshold is used by `latte--after-change-function', which is a text
  change listener. On text-changed events, Latte normally re-highlights the
  modified text only. However, when the number of characters is less than this
  threhold, Latte re-highlights the whole line instead.")

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

(defun latte--delete-overlays (&optional force start end)
  "Called internally to delete overlays that are no longer needed.

When FORCE is nil, this function goes through each overlay existing in the
current buffer, and performs some checking such as whether its underline text is
still a keyword. If it is not, this function deletes the overlay.

However, setting FORCE to t makes this function to delete all overlays without
checking."

  (setq start (or start (point-min))
		end (or end (point-max)))
  (ignore-errors
	(let ((latte--overlays (overlays-in start end)))
	  ;; For each overlay
	  (while (not (null latte--overlays))
		(let* ((o (car latte--overlays))
			   (min (overlay-start o))
			   (end (overlay-end o))
			   (f (overlay-get o 'face))
			   (keyword (s-downcase (buffer-substring-no-properties min end))))
		  ;; Make sure it belongs to latte.el.
		  (when (or force
					(and (equal f 'latte-keyword-face)
						 ;; Check if it still points at a keyword
						 (not (gethash keyword latte--keywords)))
					;; Check if it is truly the end of a word
					(save-excursion
					  (ignore-errors
						(goto-char end)
						(end-of-thing 'word)
						(not (equal end (point))))))

			;; If not
			(delete-overlay o))

		  (setq latte--overlays (cdr latte--overlays)))))))

(defun latte--overlay-exists (keyword start end)
  "Return t if an overlay for KEYWORD exists between START and END."

  (cl-loop for co in (overlays-in start end)
		do
		(when (equal keyword (overlay-get co 'latte-keyword))
		  (if (and (equal (overlay-start co) start)
				   (equal (overlay-end co) end))
			  (cl-return t)

			;; Region mismatch; e.g. an old overlay that does not
			;; accommodate the extra length. Clean it and continue searching.
			(delete-overlay co)))))

(defun latte--phrase-checker (phrase)
  "Returns t if PHRASE is a keyword."

  (gethash (latte--chop-keyword phrase) latte--keywords))

(defun latte--highlight (&optional start end)
  "Highlights all the instances of KEYWORD in the current buffer. For each
  instance, this function creates a clickable overlay.

The optional second argument START indicates starting position. Highlighting
must not occur before START. A value of nil means search from '(point-min)'.

The optional third argument END indicates ending position. Highlight must not
occur after END. A value of nil means search from '(point-max)'."

  ;; Default values
  (setq start (or start (point-min)))
  (setq end (min (or end (point-max)) (point-max)))

  (ignore-errors
	(save-mark-and-excursion
	  (with-silent-modifications
		(save-restriction
		  (narrow-to-region start end)
		  ;; Go to starting point
		  (goto-char (point-min))
		  (latte--delete-overlays nil start end)

		  (let (w ;; The word found in the current search iteration
				old-w ;; The word found in the previous search iteration
				older-w ;; The word found in two search iterations ago
				phrase
				chopped-phrase) ;; Used to hold the chopped version of PHRASE
			;; For each word
			(while (and (< (point) (point-max))
						(forward-word))

			  ;; We can't use text property face as it is over-ruled by
			  ;; font-lock highlighting. To solve this problem, we utilize
			  ;; overlays since keywords have to be updated regularly, and a
			  ;; keyword can even be part of multiple phrases. Having multiple
			  ;; overlays with different priorities on top of the keyword
			  ;; addresses these problems, given the fact that an overlay with
			  ;; high priority overshadows the lower. In addition, utilizing
			  ;; overlays reduces code complexity for re-highlighting nested
			  ;; keywords when the parent phrase is destroyed.

			  ;; When latte-highlight-prog-comments is on, overlays in prog-mode
			  ;; must be inside comment sections only
			  (unless (and latte-highlight-prog-comments
						   (derived-mode-p 'prog-mode)
						   ;; Comment section?
						   ;; from https://github.com/blorbx/evil-quickscope
						   (not (nth 4 (syntax-ppss))))

				(setq older-w old-w
					  old-w w
					  w (downcase (substring-no-properties (or (word-at-point) "")))
					  phrase nil)

				;; Search for a keyword, which can be either: a phrase that
				;; consists of two or three words, or a single word.
				(cond
				 ((setq chopped-phrase (latte--phrase-checker
										(concat older-w " " old-w " " w)))
				  (setq phrase (concat older-w " " old-w " " w)))

				 ((setq chopped-phrase (latte--phrase-checker
										(concat old-w " " w)))
				  (setq phrase (concat old-w " " w)))

				 ((setq chopped-phrase (latte--phrase-checker w))
				  (setq phrase w)))

				(when phrase
				  (let* ((l (length phrase))
						 (beginning (- (point) l))
						 (end  (point)))

					(unless (latte--overlay-exists chopped-phrase beginning end)
					  (let ((o (make-overlay beginning end)))
						(overlay-put o 'face 'latte-keyword-face)
						;; On text modification under the overlay
						(overlay-put o
									 'modification-hooks
									 '((lambda (overlay &rest args)
										 ;; delete the overlay. Re-drawing will
										 ;; occur later if the new text is still
										 ;; a member of 'latte--keywords'
										 (delete-overlay overlay))))

						(overlay-put o 'keymap latte-keyword-map)
						(overlay-put o 'mouse-face 'highlight)
						;; Use the pure form to improve the quality of the
						;; search when requested.
						(overlay-put o 'latte-keyword chopped-phrase)

						;; The priority is calculated based on the number of the
						;; characters. Thus, overlays with longer phrases are on
						;; top.
						(overlay-put o 'priority l)))))))))))))

(defun latte--chop-keyword (keyword)
  "Removes meta characters from KEYWORD such as 'ies', 'es' and 's', which are
  commonly found in plural nouns."

  (or (when (s-suffix? "ies" keyword)
		;; Use the chopped version as the value to improve the results
		;; on-the-fly latte searches.
		(s-chop-suffix "ies" keyword))
	  (when (s-suffix? "es" keyword)
		(s-chop-suffix "es" keyword))
	  (when (s-suffix? "s" keyword)
		(s-chop-suffix "s" keyword))
	  (when (and latte-predict-other-forms
				 (s-suffix? "y" keyword))
		(s-chop-suffix "y" keyword))
	  keyword))

(defun latte--add-keyword (keyword)
  "Called internally to add KEYWORD to 'latte--keywords'.

   This functions makes sure that there is no duplicated
   keywords in latte--keywords."

  ;; If it is a new keyword and not blacklisted!
  (unless (or (gethash keyword latte--keywords)
			  (member keyword latte-ignore-words))
	;; Add KEYWORD along with all possible chopped forms
	(cl-loop for k in (list keyword
						 ;; Play with '-' to address cases such as well-done and
						 ;; well done
						 (s-replace "-" " " keyword)
						 ;; '_' makes the keyword org-tag friendly. Add the
						 ;; non-friendly forms as they are mostly likely used in
						 ;; normal English writing.
						 (s-replace "_" " " keyword)
						 (s-replace "_" "-" keyword))
		  do
		  ;; Add the non-chopped version
		  (puthash k
				   k
				   latte--keywords)

		  ;; Also include the chopped form, which is more useful. During
		  ;; highlighting, the chopped form is strongly preferred as it allows
		  ;; Latte to abstract meta characters from plural nouns. For instance,
		  ;; if the original keyword is 'symbols', the chopped form allows Latte
		  ;; to highlight the word 'symbol' (singular) as well. The same can be
		  ;; said for the words 'boxes' and 'box'.
		  (puthash (latte--chop-keyword k)
				   (latte--chop-keyword k)
				   latte--keywords)

		  (when latte-predict-other-forms
			;; Handle a few well-known, special cases:
			(cond ((s-suffix? "ies" k)
				   ;; Address cases like 'baby' and 'babies'.
				   (puthash (concat (latte--chop-keyword k) "y")
							(latte--chop-keyword k)
							latte--keywords))

				  ((s-suffix? "es" k)
				   ;; Normally, 'es' should be chopped. However, there are a
				   ;; considerable amount of cases in which you need to keep the 'e',
				   ;; e.g. 'tables' and 'table'.
				   (puthash (concat (latte--chop-keyword k) "e")
							(latte--chop-keyword k)
							latte--keywords)))

			;; Include possible plural forms in case k is in its
			;; singular form.
			(unless (s-suffix? "s" k)
			  (puthash (concat k "es")
					   (latte--chop-keyword k)
					   latte--keywords)
			  (puthash (concat k "s")
					   (latte--chop-keyword k)
					   latte--keywords))))))

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
  (cl-loop for l in (s-lines str)
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
			  (cl-loop for s in (s-split ":"  latte--async-line)
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
		(latte--highlight (window-start) (window-end nil t))))))

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

	(let* ((cmd (concat "rg --no-trim --no-line-number --color never --no-line-buffered --no-messages --no-filename --only-matching "
					  " -t org '^[\\*].+[:].+[:]$' "
					  latte-directory))
		   (proc (start-process-shell-command
				  latte--process-name
				  (get-buffer-create "*Messages*")
				  cmd)))
	  (set-process-filter proc 'latte--async-filter)
	  (set-process-sentinel proc 'latte--async-sentinel))))

(defun latte--keyword-at-point()
  "Return the highlighted keyword at point."

  (let ((p (overlays-at (point) t))
		(lk nil))
	(cl-loop for o in p
		  do
		  (when-let (k (overlay-get o 'latte-keyword))
			(cl-return (setq lk (downcase k)))))
	(or lk
		(when (use-region-p)
		  (buffer-substring-no-properties (region-beginning) (region-end)))
		(word-at-point)
		"")))

(defun latte--after-change-function (beginning end &optional old-len)
  "Highlight new keywords text modification events occur."

  ;; The different between BIGINNING and END can be as small as one character.
  (when (< (- end beginning) latte--text-change-line-margin)
	;; Scan the whole line instead
	(setq beginning (line-beginning-position)
		  end (line-end-position)))
  (latte--highlight beginning end))

(defun latte--keywords-taggable ()
  "Go through 'latte--keyword' to generate a list of keywords usable as Org
tags. Spaces and '-' are replaced by '_'."

  (delete-dups
   (append (cl-loop for k in (hash-table-keys latte--keywords)
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
	(if tags
		(unless (member keyword tags)
		  (push keyword (cdr (last tags)))
		  (org-set-tags-to tags))
	  (progn
		(org-set-tags-to keyword)))))


(defun latte--insert-keyword-handler (keyword)
  "Insert KEYWORD at point. Point and after-insertion markers move forward to
  end up after the inserted text. This function is called automatically by Ivy,
  spawned in 'latte-insert-keyword'."

  (when (listp keyword)
	(setq keyword (cdr keyword)))
  (insert keyword))

(defun latte--scroll-handler (win start)
  "Called when scroll events occur."
  (let ((diff (- start latte--prev-start-win ))
		(end (window-end win t)))
	(if (> diff 0)
		;; Going down
		(progn
		  ;; Full window highlight if it is a large jump
		  (if (>= start latte--prev-end-win)
			  (latte--highlight start end)
			;; Otherwise, highlight partially
			(latte--highlight latte--prev-end-win end)))
	  ;; Going up
	  (progn
		;; Full window highlight if it is a large jump
		(if (< end latte--prev-start-win)
			(latte--highlight start end)
		  ;; Otherwise, highlight partially
		  (latte--highlight start latte--prev-start-win))))
	(setq latte--prev-start-win start
		  latte--prev-end-win end)))

;;;
;;; Minor mode
;;;

;;;###autoload
(define-minor-mode latte-mode
  "Minor mode highlights notebook's keywords throughout the buffer.

Initially, highlighting takes place after 'latte-scan-idle-delay'."
  :init-value nil
  :lighter latte
  :keymap nil
  :require 'latte
  :group 'latte

  (if latte-mode
	  ;; on
	  (progn
		;; Bound Latte highlighting to after-revert, window-scroll and after-change hooks
		(add-hook 'after-revert-hook 'latte--highlight t t)
		(add-hook 'after-change-functions 'latte--after-change-function t t)
		(add-hook 'window-scroll-functions 'latte--scroll-handler t t)
		;; On major change
		(add-hook 'change-major-mode-hook 'latte--change-major-mode t t)

		;; Check if the global timer has started
		(unless latte--initialized
		  (latte--scan-keywords)
		  (run-with-idle-timer latte-scan-idle-delay t
							   'latte--scan-keywords)
		  (setq latte--initialized t)))

	;; off
	(progn
	  ;; Remove our overlays
	  (latte--delete-overlays t)

	  ;; Remove local hooks
	  (remove-hook 'after-revert-hook 'latte--highlight t)
	  (remove-hook 'after-change-functions 'latte--after-change-function t)
	  (remove-hook 'window-scroll-functions 'latte--scroll-handler t)
	  (remove-hook 'change-major-mode-hook 'latte--change-major-mode t))))

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
  (counsel-rg init-input latte-directory "-t org"
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
