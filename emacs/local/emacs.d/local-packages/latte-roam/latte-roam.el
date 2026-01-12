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
;;; - Remove the needs for s.el.
;;;

;;; Code:

(require 's)
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

(defcustom latte-roam-scan-idle-delay 60
  "Number of seconds of idle time before re-scanning note files. If
this variable is set to 0; no idle time is taken.

Changing the value does not take effect until next Emacs reboot."
  :group 'latte-roam
  :type 'number)

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

(defconst latte-roam--text-change-line-margin 10
  "This threshold is used by `latte-roam--after-change-function', which is a text
  change listener. On text-changed events, Latte-Roam normally re-highlights the
  modified text only. However, when the number of characters is less than this
  threhold, Latte-Roam re-highlights the whole line instead.")

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
			   (keyword (s-downcase (buffer-substring-no-properties min end))))
		  ;; Make sure it belongs to latte.el.
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

  (gethash (latte-roam--chop-keyword phrase) latte-roam--keywords))

(defun latte-roam--highlight (&optional start end all-buffers)
  ;; (message "latte-roam--highlight %s %s %s" start end all-buffers)
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
	;; (message "start: make-overlays  %S %s %s %s" buffer start end  latte-roam-mode)
	;; Check if the latte-mode mode is active
	(when latte-roam-mode
	  ;; (message "latte-roam--make-overlays  %s %s %s" buffer start end)
	  (ignore-errors
		(setq start (or start (point-min))
			  end (min (or end (point-max)) (point-max)))
		(save-mark-and-excursion
		  (with-silent-modifications
			(save-restriction
			  (narrow-to-region start end)
			  ;; Go to starting point
			  (goto-char (point-min))
			  (latte-roam--delete-overlays nil start end)

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

				  ;; When latte-roam-highlight-prog-comments is on, overlays in prog-mode
				  ;; must be inside comment sections only
				  (unless (and latte-roam-highlight-prog-comments
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
					 ((setq chopped-phrase (latte-roam--phrase-checker
											(concat older-w " " old-w " " w)))
					  (setq phrase (concat older-w " " old-w " " w)))

					 ((setq chopped-phrase (latte-roam--phrase-checker
											(concat old-w " " w)))
					  (setq phrase (concat old-w " " w)))

					 ((setq chopped-phrase (latte-roam--phrase-checker w))
					  (setq phrase w)))

					(when phrase
					  (let* ((l (length phrase))
							 (beginning (- (point) l))
							 (end  (point)))

						(unless (latte-roam--overlay-exists chopped-phrase beginning end)
						  (let ((o (make-overlay beginning end)))
							;; (message "make an actual overlay")
							(overlay-put o 'face 'latte-roam-keyword-face)
							;; On text modification under the overlay
							(overlay-put o
										 'modification-hooks
										 '((lambda (overlay &rest args)
											 ;; delete the overlay. Re-drawing will
											 ;; occur later if the new text is still
											 ;; a member of 'latte-roam--keywords'
											 (delete-overlay overlay))))

							(overlay-put o 'keymap latte-roam-keyword-map)
							(overlay-put o 'mouse-face 'highlight)
							;; Use the pure form to improve the quality of the
							;; search when requested.
							(overlay-put o 'latte-roam-keyword chopped-phrase)

							;; The priority is calculated based on the number of the
							;; characters. Thus, overlays with longer phrases are on
							;; top.
							(overlay-put o 'priority l)))))))))))))))

(defun latte-roam--chop-keyword (keyword)
  "Removes meta characters from KEYWORD such as `ies', `es' and `s', which are
  commonly found in plural nouns."

  (or (when (s-suffix? "ies" keyword)
		;; Use the chopped version as the value to improve the results
		;; on-the-fly latte-roam searches.
		(s-chop-suffix "ies" keyword))
	  (when (s-suffix? "es" keyword)
		(s-chop-suffix "es" keyword))
	  (when (s-suffix? "s" keyword)
		(s-chop-suffix "s" keyword))
	  (when (and latte-roam-predict-other-forms
				 (s-suffix? "y" keyword))
		(s-chop-suffix "y" keyword))
	  keyword))

(defun latte-roam--add-keyword (keyword)
  "Called internally to add KEYWORD to `latte-roam--keywords'.

   This functions makes sure that there is no duplicated
   keywords in latte-roam--keywords."

  ;; If it is a new keyword and not blacklisted!
  (unless (or (gethash keyword latte-roam--keywords)
			  (member keyword latte-roam-ignore-words))
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
					  latte-roam--keywords)

			 ;; Also include the chopped form, which is more useful. During
			 ;; highlighting, the chopped form is strongly preferred as it allows
			 ;; Latte-Roam to abstract meta characters from plural nouns. For instance,
			 ;; if the original keyword is 'symbols', the chopped form allows Latte-Roam
			 ;; to highlight the word 'symbol' (singular) as well. The same can be
			 ;; said for the words 'boxes' and 'box'.
			 (puthash (latte-roam--chop-keyword k)
					  (latte-roam--chop-keyword k)
					  latte-roam--keywords)

			 (when latte-roam-predict-other-forms
			   ;; Handle a few well-known, special cases:
			   (cond ((s-suffix? "ies" k)
					  ;; Address cases like 'baby' and 'babies'.
					  (puthash (concat (latte-roam--chop-keyword k) "y")
							   (latte-roam--chop-keyword k)
							   latte-roam--keywords))

					 ((s-suffix? "es" k)
					  ;; Normally, 'es' should be chopped. However, there are a
					  ;; considerable amount of cases in which you need to keep the 'e',
					  ;; e.g. 'tables' and 'table'.
					  (puthash (concat (latte-roam--chop-keyword k) "e")
							   (latte-roam--chop-keyword k)
							   latte-roam--keywords)))

			   ;; Include possible plural forms in case k is in its
			   ;; singular form.
			   (unless (s-suffix? "s" k)
				 (puthash (concat k "es")
						  (latte-roam--chop-keyword k)
						  latte-roam--keywords)
				 (puthash (concat k "s")
						  (latte-roam--chop-keyword k)
						  latte-roam--keywords))))))

(defun latte-roam--kill-processes ()
  "Terminate the process launched by `latte-roam--keywords-check'."

  (let ((proc (get-process  latte-roam--process-name)))
	(when proc
	  (delete-process proc))))

(defun latte-roam--process-node (node)
  "Extract the keywords from the given `node' and update `latte-roam--keywords'
accordingly.

Currently, we are treating the title and the tags of the given node as keywords."

  (latte-roam--add-keyword (s-downcase (org-roam-node-title node))))

(defun latte-roam--scan-keywords (&rest args)
  "Uses org-roam database to updates `latte-roam--keywords'."

  (setq latte-roam--keywords (make-hash-table :test 'equal))
  (mapcar #'latte-roam--process-node
		  (org-roam-node-list))
  (latte-roam--highlight nil nil t)
  t)

(defun latte-roam--db-modified (&rest args)
  "An advice function that runs after some functions in org-roam. This function
triggers `latte-roam-scan-keywords' after some db modification."

  (latte-roam--scan-keywords))

(defun latte-roam--db-sync (org-fn &rest args)
  "An advice function that runs around `org-roam-db-sync' to trigger
`latte-roam-scan-keywords' after every db re-sync."
  ;; (message "latte-roam--db-sync %s" org-fn)
  (ignore-errors
	(prog1
		(funcall org-fn args)
	  (latte-roam--scan-keywords))))


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

(defun latte-roam--after-change-function (beginning end &optional old-len)
  "Highlight new keywords text modification events occur."

  (ignore old-len)
  ;; The different between BIGINNING and END can be as small as one character.
  (when (< (- end beginning) latte-roam--text-change-line-margin)
	;; Scan the whole line instead
	(setq beginning (line-beginning-position)
		  end (line-end-position)))
  (latte-roam--highlight beginning end))

(defun latte-roam--after-revert-function ()
  "Re-highlight keywords when revert events occur."
  (latte-roam--highlight))

(defun latte-roam--keywords-taggable ()
  "Go through `latte-roam--keyword' to generate a list of keywords usable as Org
tags. Spaces and '-' are replaced by '_'."

  (delete-dups
   (append (cl-loop for k in (hash-table-keys latte-roam--keywords)
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
  "Minor mode highlights notebook's keywords throughout the buffer.

Initially, highlighting takes place after `latte-roam-scan-idle-delay'."
  :init-value nil
  :lighter latte
  :keymap nil
  :require 'latte-roam
  :group 'latte-roam

  (if latte-roam-mode
	  ;; on
	  (progn
		;; Bound Latte-Roam highlighting to after-revert, window-scroll and after-change hooks
		(add-hook 'after-revert-hook 'latte-roam--after-revert-function t t)
		(add-hook 'after-change-functions 'latte-roam--after-change-function t t)
		(add-hook 'window-scroll-functions 'latte-roam--scroll-handler t t)
		;; On major change
		(add-hook 'change-major-mode-hook 'latte-roam--change-major-mode t t)

		;; Check if the global timer has started
		(unless latte-roam--initialized
		  (latte-roam--scan-keywords)
		  (run-with-idle-timer latte-roam-scan-idle-delay t #'latte-roam--scan-keywords)
		  (advice-add 'org-roam-db-autosync--setup-file-h :after #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-autosync--rename-file-a :after #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-autosync--delete-file-a :after  #'latte-roam--db-modified)
		  (advice-add 'org-roam-db-sync :around #'latte-roam--db-sync)


		  (setq latte-roam--initialized t)))

	;; off
	(progn
	  ;; Remove our overlays
	  (latte-roam--delete-overlays t)

	  ;; Remove local hooks
	  (remove-hook 'after-revert-hook 'latte-roam--after-revert-function t)
	  (remove-hook 'after-change-functions 'latte-roam--after-change-function t)
	  (remove-hook 'window-scroll-functions 'latte-roam--scroll-handler t)
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
