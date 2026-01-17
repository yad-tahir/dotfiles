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

(defvar latte-roam--keywords (make-hash-table :test 'equal)
  "Holds list of keywords.

This global data structure is modified primarily by `latte-roam--db-modified'.

Both `latte-roam--highlight-buffer' and `latte-roam--delete-overlays' use this list
to update UI accordingly.")

(defvar latte-roam--initialized nil
  "Holds t if notebook's timers are initialized and started. Otherwise, nil.

This variable is used to ensure only one instance of the timers exists
globally.")

(defvar-local latte-roam--prev-start-win 0
  "Holds window start position before scrolling.")

(defvar-local latte-roam--prev-end-win 0
  "Holds window end position before scrolling.")

(defvar-local latte-roam--prev-win nil
  "Holds last window object this buffer used during scrolling.")

;;;
;;; Helpers
;;;

(defun latte-roam--change-major-mode ()
  "Called internally when the major mode has changed in the current buffer."

  (latte-roam--delete-overlays nil nil t))

(defun latte-roam--delete-overlays (&optional start end force)
  "Called internally to delete overlays that are between START and END,
and no longer valid.

Goes through each overlay existing in the current buffer, and performs some
checking such as whether it still has still valid  keyword. If not, this function
deletes the overlay.

When FORCE is non-nil, keyword checking is not performed. The overlay deleted immediately."

  (setq start (or start (point-min))
		end (or end (point-max)))

  (dolist (o (overlays-in start end))
	(let* ((o-start (overlay-start o))
		   (o-end (overlay-end o))
		   (o-f (overlay-get o 'face))
		   (keyword (downcase (buffer-substring-no-properties o-start o-end))))
	  ;; Make sure it belongs to us
	  (when (or force
				(and (equal o-f 'latte-roam-keyword-face)
					 ;; Check if it still points at a keyword
					 (not (gethash keyword latte-roam--keywords))))
		(delete-overlay o)))))

(defun latte-roam--overlay-exists (keyword start end)
  "Return t if an overlay for KEYWORD exists between START and END."

  (catch 'latte-roam--overlay-found
	(dolist (co (overlays-in start end))
	  (when (equal keyword (overlay-get co 'latte-roam-keyword))
		(if (and (equal (overlay-start co) start)
				 (equal (overlay-end co) end))
			(throw 'latte-roam--overlay-found t)

		  ;; Region mismatch; e.g. an old overlay that does not
		  ;; accommodate the extra length. Clean it and continue searching.
		  (delete-overlay co)
		  nil)))))

(defun latte-roam--phrase-checker (phrase)
  "Returns t if PHRASE is a keyword."

  (gethash phrase latte-roam--keywords))

(defun latte-roam--highlight-buffers ()
  (dolist (buffer (buffer-list))
	(latte-roam--highlight-buffer nil nil buffer)))

(defun latte-roam--highlight-buffer (&optional start end buffer)
  (setq buffer (or buffer (current-buffer)))

  (with-current-buffer buffer
	(when (bound-and-true-p latte-roam-mode)
	  (when-let ((b-win (get-buffer-window)))
		(let ((b-start (or start (window-start b-win)))
			  (b-end (or end (window-end b-win t))))
		  (latte-roam--make-overlays buffer b-start b-end))))))

(defun latte-roam--make-overlays (buffer &optional start end)
  "Highlights all the instances of KEYWORD in the current buffer. For each
  instance, this function creates a clickable overlay.

The optional second argument START indicates starting position. Highlighting
must not occur before START. A value of nil means search from `(point-min)'.

The optional third argument END indicates ending position. Highlight must not
occur after END. A value of nil means search from `(point-max)'."

  (with-current-buffer buffer
	(ignore-errors
	  (setq start (or start (point-min))
			end (min (or end (point-max)) (point-max)))
	  (save-excursion
		(save-restriction
		  (with-silent-modifications
			(narrow-to-region start end)
			;; Go to starting point
			(latte-roam--delete-overlays start end)
			(maphash (lambda (key value)
					   (goto-char start)
					   (while (word-search-forward key nil t)
						 (let ((match-beg (match-beginning 0))
							   (match-end (match-end 0)))

						   (unless (or (latte-roam--overlay-exists value match-beg match-end)
									   (and (derived-mode-p 'org-mode)
											(eq (org-element-type (org-element-context)) 'link))
									   (and latte-roam-highlight-prog-comments
											(derived-mode-p 'prog-mode)
											;; Comment section?
											;; from https://github.com/blorbx/evil-quickscope
											(not (nth 4 (syntax-ppss)))))
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

					 latte-roam--keywords)))))))

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

(defun latte-roam--db-modified (&rest args)
  "Use org-roam database to updates `latte-roam--keywords'. "

  (setq latte-roam--keywords (make-hash-table :test 'equal))
  (mapcar #'(lambda (node)
			  (latte-roam--add-keyword (downcase (org-roam-node-title node))))
		  (org-roam-node-list))
  (latte-roam--highlight-buffers)
  t)

(defun latte-roam--keyword-at-point ()
  "Return the highlighted keyword at point."

  (let ((p (overlays-at (point) t))
		(lk nil))
	(catch 'latte-roam--found
	  (dolist (o p)
		(when-let (k (overlay-get o 'latte-roam-keyword))
		  (throw 'latte-roam--found (setq lk (downcase k))))))
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
	(latte-roam--highlight-buffer (line-beginning-position) (line-end-position))))

(defun latte-roam--after-revert-function (&rest args)
  "Re-highlight keywords when revert events occur."

  (latte-roam--highlight-buffer))

(defun latte-roam--scroll-handler (win start)
  "Called when window scroll events occur.

This function is also triggered when a window is just attached to a buffer."

  (let* ((start (window-start win))
		 (end (window-end win t))
		 (diff (- start latte-roam--prev-start-win))
		 (full-render (or (not latte-roam--prev-win)
						  (not (eq latte-roam--prev-win win))
						  (> (abs diff)
							 ;; Less than 1/3 of window size
							 (/ (- latte-roam--prev-end-win latte-roam--prev-start-win) 3)))))
	(cond
	 ((and (>= diff 1) ;; Moving downward
		   (not full-render))
	  (latte-roam--highlight-buffer latte-roam--prev-end-win end))
	 ((and (< diff 1) ;; Moving upward
		   (not full-render))
	  (latte-roam--highlight-buffer start latte-roam--prev-start-win))
	 (t
	  (latte-roam--highlight-buffer start end )))

	(setq latte-roam--prev-start-win start
		  latte-roam--prev-end-win end
		  latte-roam--prev-win win)))

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
	(dolist (o p)
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
		  (advice-add 'org-roam-db-update-file :after #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-insert-file :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-clear-file :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-update-file :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-sync :after #'latte-roam--db-modified)

		  (latte-roam--db-modified)
		  (setq latte-roam--initialized t)))

	(progn ;;off
	  ;; Remove our overlays
	  (latte-roam--delete-overlays nil nil t)

	  ;; Remove local hooks
	  (remove-hook 'window-scroll-functions 'latte-roam--scroll-handler t)
	  (remove-hook 'after-revert-hook 'latte-roam--after-revert-function t)
	  (remove-hook 'after-change-functions 'latte-roam--after-change-function t)
	  (remove-hook 'change-major-mode-hook 'latte-roam--change-major-mode t)))
  t)

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
