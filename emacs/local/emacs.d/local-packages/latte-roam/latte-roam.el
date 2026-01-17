;;; latte-roam.el --- Help Org roam a bit. -*- lexical-binding: t; -*-
;;;
;;;  _          _   _         ____
;;; | |    __ _| |_| |_ ___  |  _ \ ___   __ _ _ __ ___
;;; | |   / _` | __| __/ _ \ | |_) / _ \ / _` | '_ ` _ \
;;; | |__| (_| | |_| ||  __/ |  _ < (_) | (_| | | | | | |
;;; |_____\__,_|\__|\__\___| |_| \_\___/ \__,_|_| |_| |_|
;;;
;;;
;;; Summary:
;;; Latte-Roam - Auto-highlights words based on the records in org-roam's database.
;;;
;;; Author: Dr. Yad Tahir <yad (at) ieee.org>
;;; Keywords: org-roam, auto-highlighting, org-mode.
;;;
;;; Commentary:
;;; This file is part of a simple note-taking system where notes are stored
;;; in org-roam files. This package collects the titles found in the
;;; org-roam database. Latte-Roam then automatically highlights keywords when
;;; `latte-roam-mode' is active. This implementation is designed to be snappy
;;; and minimizes  UI redrawing to maintain high performance.

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
;;; - Make Latte-Roam less aggressive towards finding plural nouns.
;;;

;;; Code:

(require 'org-roam)

(defgroup latte-roam nil "A simple notebook manager with auto highlighting built on
top of the beloved Org-mode." :group 'latte-roam)

(defcustom latte-roam-directory org-roam-directory
  "Directory in which note files are stored."
  :group 'latte-roam
  :type 'directory)

(defcustom latte-roam-highlight-prog-comments t
  "If enabled (t), highlight keywords in prog-mode comment sections only."
  :group 'latte-roam
  :type 'boolean)

(defcustom latte-roam-skip-tag "skip"
  "Applying this tag to an Org header makes latte-roam skip processing it."
  :group 'latte-roam
  :type 'string)

(defcustom latte-roam-ignore-words '()
  "The words in the list will not be treated as keywords"
  :group 'latte-roam
  :type '(repeat string))

(defcustom  latte-roam-predict-other-forms t
  "If t, Latte-Roam guesses other forms associated with a keyword.

Setting this option allows Latte-Roam to add other possible word forms,
such as predicting singular and/or plural forms. For instance, Latte-Roam
adds the words `table' and `symbol' when it finds the keywords
`tables' and `symbols', respectively."
  :group 'latte-roam
  :type 'boolean)

(defvar latte-roam-keyword-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<mouse-1>") 'latte-roam-open-at-point)
	(define-key map (kbd "<RET>") 'latte-roam-open-at-point)
	(define-key map (kbd "<M-RET>") 'latte-roam--complete-at-point)
	map)

  "Keymap for highlighted keywords.")

(defface latte-roam-keyword-face
  '((t (:underline (:color "#654d4d" :style wave :position nil))))
  "Latte-Roam mode face used to highlight keywords and topic titles"
  :group 'latte-roam)


;;; Internal variables
(defvar-local latte-roam--prev-start-win 0
  "Holds window start position before scrolling.")

(defvar-local latte-roam--prev-end-win 0
  "Holds window end position before scrolling.")

(defvar latte-roam--keywords (make-hash-table :test 'equal)
  "Holds list of keywords.

This global data structure is modified primarily by `latte-roam--scan-keywords'.
Both `latte-roam--highlight' and `latte-roam--delete-overlays' use this list
to update UI accordingly.")

(defvar latte-roam--initialized nil
  "Holds t if notebook's timers are initialized and started. Otherwise, nil.

This variable is used to ensure only one instance of the timers exists
globally.")

(defconst latte-roam--process-name "*latte-roam-keyword-scanner*"
  "Holds the name of the process launched by `latte-roam--scan-keywords'.")

;;;
;;; Helpers
;;;

(defun latte-roam--change-major-mode ()
  "Called internally when the major mode has changed in the current buffer."

  ;; Kill the scanning process if it exists to avoid creating new overlays.
  (latte-roam--kill-processes)
  ;; Delete all old overlays to ensure that the 'latte-roam-highlight-prog-comments'
  ;; setting works.
  (latte-roam--delete-overlays t))

(defun latte-roam--delete-overlays (&optional force start end)
  "Called internally to delete overlays that are no longer needed.

When FORCE is nil, this function goes through each overlay existing in the
current buffer, and performs some checking such as whether its underline text is
still a keyword. If it is not, this function deletes the overlay.

However, setting FORCE to t makes this function to delete all overlays without
checking."

  (setq start (or start (point-min))
		end (or end (point-max)))
  (ignore-errors
	(let ((latte-roam--overlays (overlays-in start end)))
	  ;; For each overlay
	  (while (not (null latte-roam--overlays))
		(let* ((o (car latte-roam--overlays))
			   (min (overlay-start o))
			   (end (overlay-end o))
			   (f (overlay-get o 'face))
			   (keyword (downcase (buffer-substring-no-properties min end))))
		  ;; Make sure it belongs to us
		  (when (or force
					(and (equal f 'latte-roam-keyword-face)
						 ;; Check if it still points at a keyword
						 (not (gethash keyword latte-roam--keywords)))
					;; Check if it is truly the end of a word
					(save-excursion
					  (ignore-errors
						(goto-char end)
						(end-of-thing 'word)
						(not (equal end (point))))))

			;; If not
			(delete-overlay o))

		  (setq latte-roam--overlays (cdr latte-roam--overlays)))))))

(defun latte-roam--overlay-exists (keyword start end)
  "Return t if an overlay for KEYWORD exists between START and END."

  (cl-loop for co in (overlays-in start end)
		   do
		   (when (equal keyword (overlay-get co 'latte-roam-keyword))
			 (if (and (equal (overlay-start co) start)
					  (equal (overlay-end co) end))
				 (cl-return t)

			   ;; Region mismatch; e.g. an old overlay that does not
			   ;; accommodate the extra length. Clean it and continue searching.
			   (delete-overlay co)))))

(defun latte-roam--phrase-checker (phrase)
  "Returns t if PHRASE is a keyword."

  (gethash phrase latte-roam--keywords))

(defun latte-roam--highlight (&optional start end all-buffers)
  (if all-buffers
	  (dolist (buffer (buffer-list))
		(latte-roam--make-overlays buffer start end))
	(latte-roam--make-overlays (current-buffer) start end)))


(defun latte-roam--make-overlays (buffer &optional start end)
  "Highlights all the instances of KEYWORD in the current buffer. For each
  instance, this function creates a clickable overlay.

The optional second argument START indicates starting position. Highlighting
must not occur before START. A value of nil means search from `(point-min)'.

The optional third argument END indicates ending position. Highlight must not
occur after END. A value of nil means search from `(point-max)'."

  (with-current-buffer buffer
	;; Check if the latte-mode mode is active
	(when latte-roam-mode
	  (ignore-errors
		(setq start (or start (point-min))
			  end (min (or end (point-max)) (point-max)))
		(save-mark-and-excursion
		  (with-silent-modifications
			(save-restriction
			  (narrow-to-region start end)
			  ;; Go to starting point
			  (latte-roam--delete-overlays nil start end)
			  (maphash (lambda (key value)
						 (goto-char start)
						 (while (word-search-forward key nil t)
						   (let ((match-beg (match-beginning 0))
								 (match-end (match-end 0)))

							 (unless (and latte-roam-highlight-prog-comments
										  (derived-mode-p 'prog-mode)
										  ;; Comment section?
										  ;; from https://github.com/blorbx/evil-quickscope
										  (not (nth 4 (syntax-ppss)))
										  (latte-roam--overlay-exists value match-beg match-end))
							   (let ((o (make-overlay match-beg match-end)))
								 (overlay-put o 'face 'latte-roam-keyword-face)

								 ;; Vanish when empty (deleted text):
								 (overlay-put o 'evaporate t)

								 (overlay-put o 'keymap latte-roam-keyword-map)
								 (overlay-put o 'mouse-face 'highlight)
								 ;; Use the pure form to improve the quality of the
								 ;; search when requested.
								 (overlay-put o 'latte-roam-keyword value)

								 ;; The priority is calculated based on the number of the
								 ;; characters. Thus, overlays with longer phrases are on
								 ;; top.
								 (overlay-put o 'priority (length key)))))))

					   latte-roam--keywords))))))))

(defun latte-roam--pluralize (phrase)
  "Return the plural form of PHRASE using standard English grammar rules.

Handles simple phrases like 'inverse element' -> 'inverse elements'."
  (let ((case-fold-search t))
	(cond
	 ;; Common irregulars
	 ((string-equal phrase "child") "children")
	 ((string-equal phrase "person") "people")
	 ((string-equal phrase "man") "men")
	 ((string-equal phrase "woman") "women")
	 ((string-equal phrase "tooth") "teeth")
	 ((string-equal phrase "foot") "feet")

	 ;; Words ending in s, x, z, ch, sh
	 ;; FIX: Wrapped in \\(...\\) so $ applies to all alternatives
	 ((string-match-p "\\([sxz]\\|ch\\|sh\\)$" phrase)
	  (concat phrase "es"))

	 ;; Words ending in consonant + y
	 ((string-match-p "[^aeiou]y$" phrase)
	  (concat (substring phrase 0 -1) "ies"))

	 ;; Words ending in f or fe; remove f/fe, add "ves"
	 ((string-match-p "\\(li\\|wi\\|lo\\|lea\\|shel\\|thie\\)fe?$" phrase)
	  (replace-regexp-in-string "fe?$" "ves" phrase))

	 (t (concat phrase "s")))))

(defun latte-roam--add-keyword (keyword)
  "Called internally to add KEYWORD to `latte-roam--keywords'.

   This functions makes sure that there is no duplicated
   keywords in latte-roam--keywords."

  ;; If it is a new keyword and not blacklisted!
  (unless (or (gethash keyword latte-roam--keywords)
			  (member keyword latte-roam-ignore-words))
	;; Add KEYWORD along with all possible chopped forms
	(puthash keyword keyword latte-roam--keywords)
	(puthash (latte-roam--pluralize keyword) keyword latte-roam--keywords)))

(defun latte-roam--kill-processes ()
  "Terminate the process launched by `latte-roam--keywords-check'."

  (let ((proc (get-process  latte-roam--process-name)))
	(when proc
	  (delete-process proc))))

(defun latte-roam--process-node (node)
  "Extract the keywords from the given `node' and update `latte-roam--keywords'
accordingly.

Currently, we are treating the title and the tags of the given node as keywords."

  (latte-roam--add-keyword (downcase (org-roam-node-title node))))

(defun latte-roam--scan-keywords (&rest args)
  "Uses org-roam database to updates `latte-roam--keywords'."

  (setq latte-roam--keywords (make-hash-table :test 'equal))
  (mapcar #'latte-roam--process-node
		  (org-roam-node-list))
  (latte-roam--highlight nil nil t)
  t)

(defun latte-roam--db-modified (&rest args)
  "Runs after some functions in org-roam. This function
triggers `latte-roam-scan-keywords' after some db modification."

  (latte-roam--scan-keywords))

(defun latte-roam--keyword-at-point ()
  "Return the highlighted keyword at point."

  (let ((p (overlays-at (point) t))
		(lk nil))
	(cl-loop for o in p
			 do
			 (when-let (k (overlay-get o 'latte-roam-keyword))
			   (cl-return (setq lk (downcase k)))))
	(or lk
		(when (use-region-p)
		  (buffer-substring-no-properties (region-beginning) (region-end)))
		(word-at-point)
		"")))

(defun latte-roam--after-change-function (beginning end &optional _old-len)
  "Highlight new keywords text modification events occur."
  (save-excursion
	(goto-char beginning)
	;; redraw the entire line
	(latte-roam--highlight (line-beginning-position) (line-end-position))))

(defun latte-roam--after-revert-function (&rest args)
  "Re-highlight keywords when revert events occur."
  (latte-roam--highlight))

(defun latte-roam--keywords-taggable ()
  "Go through `latte-roam--keywords' to generate a list of keywords usable as Org
tags. Spaces and '-' are replaced by '_'."
  (delete-dups
   (append (cl-loop for k in (hash-table-keys latte-roam--keywords)
					collect
					;; Replaces both spaces and hyphens with underscores in one go
					(replace-regexp-in-string "[ -]" "_" k))
		   (list latte-roam-skip-tag))))

(defun latte-roam--scroll-handler (win start)
  "Called when scroll events occur."
  (let ((diff (- start latte-roam--prev-start-win ))
		(end (window-end win t)))
	(if (> diff 0)
		;; Going down
		(progn
		  ;; Full window highlight if it is a large jump
		  (if (>= start latte-roam--prev-end-win)
			  (latte-roam--highlight start end)
			;; Otherwise, highlight partially
			(latte-roam--highlight latte-roam--prev-end-win end)))
	  ;; Going up
	  (progn
		;; Full window highlight if it is a large jump
		(if (< end latte-roam--prev-start-win)
			(latte-roam--highlight start end)
		  ;; Otherwise, highlight partially
		  (latte-roam--highlight start latte-roam--prev-start-win))))
	(setq latte-roam--prev-start-win start
		  latte-roam--prev-end-win end)))

(defun latte-roam--node-insert (keyword)
  (unwind-protect
	  ;; Group functions together to avoid inconsistent state on quit
	  (atomic-change-group
		(let* (region-text
			   beg end
			   (_ (when (region-active-p)
					(setq beg (set-marker (make-marker) (region-beginning)))
					(setq end (set-marker (make-marker) (region-end)))
					(setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
			   (node (org-roam-node-read keyword nil))
			   (description (or region-text
								(org-roam-node-formatted node))))
		  (when (org-roam-node-id node)
			(progn
			  (when region-text
				(delete-region beg end)
				(set-marker beg nil)
				(set-marker end nil))
			  (let ((id (org-roam-node-id node)))
				(insert (org-link-make-string
						 (concat "id:" id)
						 description))
				(run-hook-with-args 'org-roam-post-node-insert-hook
									id
									description))))))
	(deactivate-mark)))

(defun latte-roam--complete-at-point ()
  "Places a link for the node for the overlay at point."

  (interactive)
  (let ((p (overlays-at (point) t))
		(start nil)
		(end nil)
		(keyword nil)
		(ignore nil))
	(cl-loop for o in p do
			 (unless ignore
			   (when-let (k (overlay-get o 'latte-roam-keyword))
				 (setq keyword k
					   start (overlay-start o)
					   end (overlay-end o)
					   ignore t))))

	(save-excursion
	  (set-mark start)
	  (goto-char end)
	  (latte-roam--node-insert keyword))))

;;;
;;; Minor mode
;;;

;;;###autoload
(define-minor-mode latte-roam-mode
  "Minor mode highlights notebook's keywords throughout the buffer."

  :init-value nil
  :lighter latte
  :keymap nil
  :require 'latte-roam
  :group 'latte-roam

  (if latte-roam-mode
	  (progn ;;on
		;; Local hooks
		(add-hook 'window-scroll-functions 'latte-roam--scroll-handler t t)
		(add-hook 'after-revert-hook 'latte-roam--after-revert-function t t)
		(add-hook 'after-change-functions 'latte-roam--after-change-function t t)
		(add-hook 'change-major-mode-hook 'latte-roam--change-major-mode t t)

		;; Globally exec once
		(unless latte-roam--initialized
		  (latte-roam--scan-keywords)
		  (advice-add 'org-roam-db-update-file :after #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-insert-file :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-clear-all :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-clear-file :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-update-file :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-sync :after #'latte-roam--db-modified)

		  (setq latte-roam--initialized t)))

	(progn ;;off
	  ;; Remove our overlays
	  (latte-roam--delete-overlays t)

	  ;; Remove local hooks
	  (remove-hook 'window-scroll-functions 'latte-roam--scroll-handler t)
	  (remove-hook 'after-revert-hook 'latte-roam--after-revert-function t)
	  (remove-hook 'after-change-functions 'latte-roam--after-change-function t)
	  (remove-hook 'change-major-mode-hook 'latte-roam--change-major-mode t)))
  t)

;;;###autoload
(defun latte-roam-grep (&optional init-input)
  "Interactively search through the notes' text. INIT-INPUT can be passed as the
  initial grep query."

  (interactive)
  ;; Pass a regex to ask ag to discard org metadata.
  ;;^[] beginning of the line
  ;;[^] not
  ;; * zero or more char
  ;;(counsel-ag "^[^#]\|[ ]*[^:] " "~/notes" "--nomultiline" )
  (unless (fboundp 'counsel)
	(require 'counsel))
  (setq init-input (or init-input ""))
  (counsel-rg init-input latte-roam-directory "-t org"
			  "In-text Search "))

;;;###autoload
(defun latte-roam-files ()
  "Gets list of note files along with their headers."
  (interactive)
  (let ((counsel-fzf-cmd "rg --color never --files -g '*%s*'"))
	(counsel-fzf nil latte-roam-directory "Note files ")))

;;;###autoload
(defun latte-roam-open-at-point ()
  "Search for the keyword at point in the notebook, and then show all the
  headings in which the keyword has been used."

  (interactive)
  (let* ((context (org-element-context))
		 (type (org-element-type context)))
	(if (memq type '(link))
		(org-open-at-point)
	  (org-roam-node-find nil (latte-roam--keyword-at-point)))))


(provide 'latte-roam)

;;; latte.el ends here
