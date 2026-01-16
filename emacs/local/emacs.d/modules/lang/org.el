;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(use-package org
  :defer t
  :functions (switch-to-buffer@check-org-mode org-latex-preview org-clear-latex-preview)
  :init
  (defconst do--org-directory (expand-file-name "~/notes/"))
  (defconst do--org-files-location (concat do--org-directory "journal/"))
  (defconst do--org-todos-location (concat do--org-files-location "todos.org"))
  (defconst do--org-goals-location (concat do--org-files-location "goals.org"))
  (defconst do--org-spending-location (concat do--org-files-location "spending.org"))
  (defconst do--org-journal-files (list do--org-todos-location do--org-goals-location do--org-spending-location))

  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   ;; make a prefix-command and add description
   "SPC at" '((lambda()
				(interactive)
				(find-file do--org-todos-location))
			  :which-key "todos.org"))
  :config
  ;; Remove compiler warnings
  (eval-when-compile
	(require 'org)
	(require 'org-id)
	(require 'org-habit)
	(require 'org-attach)
	(require 'org-agenda)
	(require 'org-capture))

  (general-define-key
   :keymaps 'org-mode-map
   "<RET>" 'nil
   "<M-return>" 'nil)

  (general-define-key
   :keymaps 'org-mode-map
   :states 'normal
   "TAB" 'org-cycle
   "C-M-i" 'org-cycle
   "ll" 'do-insert-attachment-link
   "lL" 'org-insert-link
   "M-c" 'org-metaup
   "M-t" 'org-metadown
   "M-h" 'org-metaleft
   "M-n" 'org-metaright
   "<RET>" 'org-open-at-point
   "<M-RET>" '(lambda ()
				(interactive)
				(evil-end-of-line)
				(goto-char (+ 1 (point)))
				(org-meta-return)
				(evil-insert-state))
   "C-c" 'org-shiftup
   "C-t" 'org-shiftdown
   "C-h" 'org-shiftleft
   "C-n" 'org-shiftright
   "M-C" 'org-shiftmetaup
   "M-T" 'org-shiftmetadown
   "M-H" 'org-shiftmetaleft
   "M-N" 'org-shiftmetaright
   "gC"  'org-up-element
   "gN"  'org-forward-element
   "gH"  'org-backward-element
   "gT"  'org-down-element
   "lt"  '(:ignore t :which-key "time")
   "ltt" '((lambda () (interactive) (org-time-stamp (current-time)))
		   :which-key "org-time-stamp")
   "ltT" '((lambda () (interactive) (org-time-stamp (current-time) t))
		   :which-key "org-time-stamp-inactive")
   "lt[" 'org-clock-in
   "lt]" 'org-clock-out
   "ltq" 'org-clock-cancel
   "ltg" 'org-clock-goto
   "lv" 'do-org-toggle-latex-preview
   "lV" '((lambda ()
			"Toggles between org-columns mode and not"
			(interactive)
			(unless (boundp 'do--org-columns-status)
			  (with-eval-after-load 'org-colview
				(setq org-columns-map (make-sparse-keymap))
				(general-define-key
				 :keymaps 'org-columns-map
				 "C-+" 'org-columns-widen
				 "C--" 'org-columns-narrow
				 "lk" '(:ignore t :which-key "columns")
				 "lk <RET>" 'org-columns-show-value
				 "lk <tab>" 'org-columns-content
				 "lk+" 'org-columns-widen
				 "lk-" 'org-columns-narrow
				 "lkd" 'org-columns-delete
				 "lku" 'org-columns-edit-attributes
				 "lkE" 'org-columns-edit-allowed
				 "lke" 'org-columns-edit-value
				 "lkn" 'org-columns-new
				 "M-h" 'org-columns-move-left
				 "M-n" 'org-columns-move-right))
			  (setq do--org-columns-status nil))
			(if (eq do--org-columns-status t)
				(progn
				  (setq do--org-columns-status nil)
				  (org-columns-quit))
			  (progn
				(setq do--org-columns-status t)
				(org-columns))))
		  :which-key "org-columns")
   "ls" 'org-schedule
   "ld" 'org-deadline
   "lp" 'org-todo
   "lu" 'org-add-note
   "l:" 'org-property-action
   ;; "l:"  'org-set-property
   "la" 'org-attach
   "lb" 'org-dynamic-block-insert-dblock
   "lg" 'org-open-at-point
   "lc" 'org-ctrl-c-ctrl-c
   "l#" 'org-id-get-create
   "l@" 'org-toggle-ordered-property
   "lel" 'org-export-dispatch
   "lep" 'org-latex-export-to-pdf
   "leb" 'org-beamer-export-to-pdf
   "leh" 'org-html-export-to-html
   "SPC lw" 'org-ctrl-c-ctrl-c ;; needed adding notes to TODOs
   "SPC lq" 'org-kill-note-or-show-branches ;; needed adding notes to TODOs
   "l'"  'org-edit-src-code
   "lx" 'org-archive-subtree)

  (general-define-key
   :keymaps 'org-src-mode-map
   :states '(normal visual)
   "SPC lw" 'org-edit-src-exit
   "SPC lq" 'org-edit-src-abort)

  (setq org-modules '(org-id
					  ;; org-bibtex org-crypt org-docview
					  ;; org-gnus
					  org-habit
					  ;; org-info org-irc org-mhe
					  ;; org-rmail
					  ;; org-drill
					  ;; org-w3m
					  )
		org-directory do--org-directory
		org-id-locations-file (concat org-directory "org-id-location")
		org-default-notes-file do--org-todos-location
		org-id-track-globally t
		org-startup-with-inline-images t
		org-startup-indented nil
		org-attach-use-inheritance t
		org-attach-auto-tag nil ;; To allow attaching files to headless blocks
		org-attach-store-link-p 'attached
		org-preview-latex-default-process 'dvisvgm
		org-startup-with-latex-preview t
		org-image-actual-width nil
		org-startup-folded t
		org-log-into-drawer "LOGBOOK"
		org-startup-folded "OVERVIEW"
		org-refile-allow-creating-parent-nodes 'confirm
		org-log-done 'time ;; Every time we close a todo, org will add
		org-todo-keywords '((sequence "TODO(t!)" "STARTED(s!)" "WAITING(w@)"
									  "|" "CANCELED(c@)" "DONE(d@)"))
		org-default-priority ?C
		org-src-fontify-natively t
		;; Ordered tasks
		org-track-ordered-property-with-tag t ;; Add :ORDER:
		org-enforce-todo-checkbox-dependencies t ;; Don't allow the super task
		org-enforce-todo-dependencies t ;; Don't allow the super task to close
		org-deadline-warning-days 7
		org-habit-following-days 1
		org-habit-preceding-days 30
		org-habit-graph-column 65
		org-priority-faces '((?A . (:inherit font-lock-type-face :weight bold))
							 (?B . (:inherit font-lock-constant-face :weight normal))
							 (?C . (:inherit nil :weight normal :slant normal))
							 (?D . (:inherit org-block :weight normal))))
  ;; Latex
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :scale 1.65)
  (plist-put org-format-latex-options :html-scale 1.65)

  ;; Open in a current window instead of a new frame
  ;; Source: https://bit.ly/2WX44mj
  (define-advice org-attach (:around (org-fn &rest args))
	(let ((pop-up-frames 'nil))
	  (apply org-fn args)))

  (define-advice org-add-log-note (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '(display-buffer-at-bottom))
		  (pop-up-frames 'nil))
	  (apply org-fn args)))

  (define-advice calendar (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '(display-buffer-at-bottom))
		  (pop-up-frames 'nil))
	  (apply org-fn args)))

  ;; Bug-Fix: Remove keybinding conflicts
  (define-advice org-completing-read (:around (org-fn &rest args))
	(let ((minibuffer-local-completion-map
		   (copy-keymap minibuffer-local-completion-map)))
	  ;; The keybinding 'C-c !' is hard coded in org.el.gz. Thus, the C-c prefix
	  ;; needs to be free. As far as I see, this has no effect on Ivy.
	  (general-define-key
	   :keymaps 'minibuffer-local-completion-map
	   "C-c" 'nil)
	  (apply org-fn args)))

  (define-advice org-compile-file (:around (org-fn source process ext &optional err-msg log-buf spec &rest args))
	(ignore log-buf)
	;; Disable log buffers to the '*Messages*' buffer to avoid creating pop-up
	;; frames
	(apply org-fn source process ext err-msg
		   (get-buffer-create "*Messages*") spec args))

  (advice-add 'org-insert-link
			  :after (lambda (&rest args)
					   (with-no-warnings
						 (ignore args)
						 (org-redisplay-inline-images))))

  (defun do-insert-attachment-link ()
	(interactive)
	(let ((org-link-parameters '(("attachment" :follow org-attach-follow :complete org-attach-complete-link))))
	  (org-insert-link)))

  ;; Avoid data corruption by auto saving after every refile operation
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Auto revert journal files if any modifications are detected
  (defun do--auto-revert-journal-files ()
	(let ((file-name (buffer-file-name)))
	  (when (and file-name
				 (string-prefix-p file-name do--org-files-location))
		(auto-revert-mode 1))))
  (add-hook 'find-file-hook 'do--auto-revert-journal-files))

(use-package org-capture
  :commands (org-capture)
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC ac" 'org-capture)

  (defun do-capture ()
	(interactive)
	(do-make-frame "capture")
	(org-capture))

  :config
  (general-define-key
   :keymaps 'org-capture-mode-map
   :states '(normal visual)
   "SPC lq" 'org-capture-kill
   "SPC lw" 'org-capture-finalize)

  (setq
   org-capture-templates
   ;; %? the initial position of the cursor
   ;; %^g prompt for tags
   ;; %^t prompt for a date
   ;; %^L prompt for a link
   ;; %i place the selected text in the other window
   ;; %^{something} prompt for string
   ;; Further details can be found at
   ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
   '(("p" "Personal TODO" entry
	  (file+olp do--org-todos-location "Personal" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("P" "Personal TODO with reference" entry
	  (file+olp do--org-todos-location "Personal" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :REF:  %^{Reference}\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("a" "AUIS TODO" entry
	  (file+olp do--org-todos-location  "AUIS" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("A" "AUIS TODO with reference" entry
	  (file+olp do--org-todos-location  "AUIS" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :REF:  %^{Reference}\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("h" "Home TODO" entry
	  (file+olp do--org-todos-location  "Home" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("H" "Home TODO with reference" entry
	  (file+olp do--org-todos-location  "Home" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :REF:  %^{Reference}\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("o" "Other TODO" entry
	  (file+olp do--org-todos-location  "Other" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("s" "Spending record" entry
	  (file+datetree do--org-spending-location)
	  "* %?\n SCHEDULED:%^u\n  :PROPERTIES:\n  :AMOUNT:  %^{Amount|0}\n  :CURRENCY: USD\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("S" "Spending record with reference" entry
	  (file+datetree do--org-spending-location)
	  "* %?\n SCHEDULED:%^u\n  :PROPERTIES:\n  :REF:  %^{Reference}\n  :AMOUNT:  %^{Amount|0}\n  :CURRENCY: USD\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)))

  ;; Bug-Fix: the face of the saved captured text will be drawn with a shadow color
  ;; because the whitespace mode is bugged out.
  (define-advice switch-to-buffer (:after (&rest _args) check-org-mode)
	(when (derived-mode-p 'org-mode)
	  (whitespace-mode 0)
	  (whitespace-mode 1)))

  ;; Bug-Fix: Force loading the normal map of the org-capture-mode map.
  ;; By default, the map is overshadowed in my setup
  (add-hook 'org-capture-mode-hook (lambda ()
									 (evil-change-state 'insert)
									 (evil-change-state 'normal)))

  (define-advice org-capture (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '((display-buffer-same-window)
											  (inhibit-same-window . nil))))
	  (apply org-fn args))))

(use-package org-agenda
  :commands (org-agenda org-agenda-list)
  :init
  (defvar do--org-journal-files nil)
  (defun do-agenda ()
	(interactive)
	(require 'org-agenda)
	(org-agenda nil "d"))

  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC a" '(:ignore t :which-key "agenda")
   "SPC aa" 'do-agenda
   "SPC aA" 'org-agenda)

  :config
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states 'normal
   "<RET>" 'org-agenda-switch-to
   "l <RET>" #'((lambda()(interactive)(do-make-frame)(org-agenda-switch-to))
				:which-key "org-agenda-switch-new-frame")
   "<f5>" 'org-agenda-redo
   ;; Ordering
   "M-t" 'org-agenda-drag-line-forward
   "M-c" 'org-agenda-drag-line-backward
   "M-h" 'org-agenda-do-date-earlier
   "M-n" 'org-agenda-do-date-later
   ;; Navigation
   "C-H" 'org-agenda-earlier
   "C-N" 'org-agenda-later
   ;; Shift keys
   "C-C" 'org-agenda-priority-up
   "C-T" 'org-agenda-priority-down
   ;; Go to
   "lF" 'org-agenda-follow-mode
   "lj" 'org-agenda-goto-date
   "lJ" 'org-agenda-goto-today
   ;; View mode
   "lv" 'nil
   "lvd" 'org-agenda-day-view
   "lvw" 'org-agenda-week-view
   "lvm" 'org-agenda-month-view
   "lvy" 'org-agenda-year-view
   "lvf" 'org-agenda-fortnight-view
   "lvD" 'org-agenda-toggle-diary
   "lvl" 'org-agenda-log-mode
   "lvc" 'org-agenda-clockreport-mode
   "lvh" 'org-agenda-holidays
   "lvS" 'org-agenda-toggle-deadlines
   "lvt" 'org-agenda-toggle-time-grid
   "lve" 'org-agenda-entry-text-mode
   "lvq" 'org-agenda-reset-view
   ;; Searching
   "l/" 'nil
   "l/#" 'org-agenda-filter-by-tag
   "l//" 'evil-ex-search-forward
   "l/?" 'evil-ex-search-backward
   "l/%" 'org-agenda-filter-by-regexp
   "l/c" 'org-agenda-filter-by-category
   "l/h" 'org-agenda-filter-by-top-headline
   "l/e" 'org-agenda-filter-by-effort
   "l/q" 'org-agenda-filter-remove-all
   ;; Extra actions
   "z" 'org-agenda-undo
   "C-z" 'org-agenda-redo
   "m" 'org-agenda-bulk-mark
   "u" 'org-agenda-bulk-unmark
   "l*" 'nil
   "l**" 'org-agenda-bulk-mark-all
   "l*t" 'org-agenda-bulk-toggle-all
   "l*%" 'org-agenda-bulk-mark-regexp
   "l*a" 'org-agenda-bulk-action
   "l*!" 'org-agenda-bulk-unmark-all
   "lp" 'org-agenda-todo
   "lu" 'org-agenda-add-note
   "lb" 'org-agenda-kill
   "l#" 'org-agenda-set-tags
   "la" 'org-attach
   "ls" 'org-agenda-schedule
   "ld" 'org-agenda-deadline
   "lg" 'org-agenda-open-link
   "lt" 'nil
   "lt[" 'org-agenda-clock-in
   "lt]" 'org-agenda-clock-out
   "ltq" 'org-agenda-clock-cancel
   "ltg" 'org-agenda-clock-goto
   "lx" 'org-agenda-archive)

  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states 'insert
   ;; Ordering
   "M-t" 'org-agenda-drag-line-forward
   "M-c" 'org-agenda-drag-line-backward
   "M-h" 'org-agenda-do-date-earlier
   "M-n" 'org-agenda-do-date-later)

  (setq org-agenda-skip-deadline-if-done t
		org-agenda-tags-column 0
		org-agenda-remove-tags t
		org-agenda-dim-blocked-tasks t
		org-agenda-weekend-days '(5 6)
		org-agenda-start-on-weekday 0
		;; org-agenda-start-with-log-mode t ;; to include all log events in time gride
		org-agenda-log-mode-items '(state)
		org-agenda-bulk-mark-char "*"
		org-agenda-use-time-grid t
		org-agenda-current-time-string "[now]"
		;; This is a work around to fix the UI because Org Agenda uses
		;; `window-width` even if the display-line-numbers mode is on.
		org-agenda-block-separator ""
		org-agenda-files do--org-journal-files)
  (add-to-list 'org-agenda-custom-commands
			   '("d" "Daily View"
				 ((agenda "Today Overview"
						  ((org-agenda-start-day "0d")
						   (org-agenda-span 2)
						   ;; (org-agenda-overriding-arguments '((4) "today" 1))  ;; lock the navigation
						   ;; (org-agenda-search-headline-for-time nil) ;; ignore time in title
						   (org-agenda-grid-show-always t)
						   (org-agenda-time-grid '((daily today) () "" "")) ;; Clear it first
						   (org-agenda-time-grid
							'((daily today)
							  (800 1000 1200 1400 1600 1800 2000 2200)
							  " ----- "
							  ""))
						   (org-agenda-overriding-header "Agenda")
						   (org-agenda-skip-function '(lambda (&rest args)
														(let ((result (or (org-agenda-skip-entry-if 'regexp "*\\* WAITING ")
																		  (org-agenda-skip-entry-if 'regexp "*\\* CANCELED ")
																		  (org-agenda-skip-entry-if 'regexp "*\\* DONE "))))
														  result)))))
				  (todo "WAITING"
						((org-agenda-start-with-log-mode '(closed))
						 (org-agenda-sorting-strategy '(timestamp-down))
						 (org-agenda-overriding-header "Waiting")))
				  (todo "DONE|CANCELED"
						((org-agenda-start-with-log-mode '(closed))
						 (org-agenda-sorting-strategy '(tsia-down))
						 (org-agenda-overriding-header "Finished Tasks")))
				  ;; (todo "DONE|CANCELED"
				  ;;		((org-agenda-start-with-log-mode '(closed))
				  ;;		 (org-agenda-sorting-strategy '(tsia-down))
				  ;;		 (org-agenda-overriding-header "Finished Goals")
				  ;;		 (org-agenda-files (file-expand-wildcards do--org-goals-location))))
				  )))


  (define-advice org-agenda (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '((display-buffer-same-window)
											  (inhibit-same-window . nil))))
	  (apply org-fn args)
	  ;; To forcefully enable text-scale-mode
	  (text-scale-set 0)))

  (define-advice org-agenda-date-prompt (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '(display-buffer-at-bottom))
		  (pop-up-frames 'nil))
	  (apply org-fn args))))

(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode)))

(use-package org-modern
  :ensure t
  :disabled t
  :hook ((org-mode . org-modern-mode)))

(use-package org-download
  :ensure t
  :after org
  :config
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   "lI" 'org-download-clipboard
   "li" 'org-download-screenshot)
  (setq org-download-method 'attach
		org-download-screenshot-method "scrot -s %s")

  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(provide 'do-org)
