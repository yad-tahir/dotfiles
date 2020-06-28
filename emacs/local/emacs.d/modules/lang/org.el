;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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
  :init
  (defconst do--org-files-location "~/notes/")
  (defconst do--org-todo-location (concat do--org-files-location "/todo.org"))
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   ;; make a prefix-command and add description
   "SPC aA" '((lambda()
				(interactive)
				(find-file do--org-todo-location))
			  :which-key "todo.org"))

  (custom-set-variables
   '(org-format-latex-options
	 `(:foreground ,chocolate-theme-white
				   :background ,chocolate-theme-bg
				   :scale 1.8
				   :html-scale 1.8)))
  :config
  ;; Remove compiler warnings
  (eval-when-compile
	(require 'org)
	(require 'org-id)
	(require 'org-habit)
	(require 'org-agenda)
	(require 'org-capture))

  (general-define-key
   :keymaps 'org-mode-map
   "<return>" 'nil
   "<M-return>" 'nil)

  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   "TAB" 'org-cycle
   "C-M-i" 'org-cycle
   "ll" 'org-insert-link
   "lp" 'org-todo
   "l#" 'org-set-tags
   "M-c" 'org-metaup
   "M-t" 'org-metadown
   "M-h" 'org-metaleft
   "M-n" 'org-metaright)

  (general-define-key
   :keymaps 'org-mode-map
   :states 'normal
   "<RET>" '(lambda ()
			  (interactive)
			  (evil-end-of-line)
			  (goto-char (+ 1 (point)))
			  (org-meta-return)
			  (evil-insert-state))
   "<M-RET>" '(lambda ()
				(interactive)
				(evil-first-non-blank)
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
		   :which-key "insert-timestamp")
   "lt(" 'org-clock-in
   "lt)" 'org-clock-out
   "ltq" 'org-clock-cancel
   "ltg" 'org-clock-goto
   "lv"  '(:ignore t :which-key "view")
   "lvl" 'org-latex-preview
   "lvc" '((lambda ()
			 "Toggles between org-columns mode and not"
			 (interactive)
			 (unless (boundp 'do--org-columns-status)
			   (with-eval-after-load 'org-colview
				 (setq org-columns-map (make-sparse-keymap))
				 (general-define-key
				  :keymaps 'org-columns-map
				  "C-+" 'org-columns-widen
				  "C--" 'org-columns-narrow
				  "l+" 'org-columns-widen
				  "l-" 'org-columns-narrow
				  "<enter>" 'org-columns-show-value
				  "ld" 'org-columns-delete
				  "lu" 'org-columns-edit-attributes
				  "lE" 'org-columns-edit-allowed
				  "le" 'org-columns-edit-value
				  "ln" 'org-columns-new
				  "l <tab>" 'org-columns-content
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
   "ls"  'org-schedule
   "lS"  'org-deadline
   "lu"  'org-add-note ;;progress
   "la"  'org-attach
   "lg"  'org-open-at-point
   "lc"  'org-ctrl-c-ctrl-c
   "li"  'org-id-get-create
   "l@"  'org-toggle-ordered-property
   "lel" 'org-export-dispatch
   "lep" 'org-latex-export-to-pdf
   "leb" 'org-beamer-export-to-pdf
   "leh" 'org-html-export-to-html
   "SPC lw" 'org-ctrl-c-ctrl-c
   "SPC lq" 'org-kill-note-or-show-branches
   "l'"  'org-edit-src-code
   "ld" 'org-archive-subtree)

  (general-define-key
   :keymaps 'org-src-mode-map
   :states '(normal visual)
   "SPC lw" 'org-edit-src-exit
   "SPC lq" 'org-edit-src-abort)

  (setq org-modules '(org-bbdb org-bibtex org-crypt org-docview
							   org-gnus org-habit org-id
							   org-info org-irc org-mhe
							   ;; org-rmail
							   ;; org-drill
							   org-w3m)
		org-startup-with-inline-images t
		org-directory do--org-files-location
		org-id-track-globally t
		org-id-locations-file (concat org-directory "org-id-location")
		org-default-notes-file do--org-todo-location
		org-startup-indented nil
		org-preview-latex-default-process 'dvisvgm
		org-startup-with-latex-preview t
		org-startup-folded t
		org-log-into-drawer "LOGBOOK"
		org-refile-allow-creating-parent-nodes 'confirm
		org-log-done 'time ;;every time we close a todo, org will add
		org-src-fontify-natively t
		;; Ordered tasks
		org-track-ordered-property-with-tag t ;; Add :ORDER:
		org-enforce-todo-checkbox-dependencies t ;; Don't allow the super task
		org-enforce-todo-dependencies t ;; Don't allow the super task to close
		org-habit-graph-column 80)

  (add-hook 'org-mode-hook 'do-line-numbers-to-visual)

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
	(apply org-fn
		   source
		   process
		   ext
		   err-msg
		   (get-buffer-create "*Messages*")
		   spec
		   args))

  ;; Theme
  (set-face-attribute 'org-special-keyword nil
					  :inherit 'font-lock-builtin-face)
  (set-face-attribute 'org-level-1 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-level-2 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-level-3 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-level-4 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-level-5 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-level-6 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-level-7 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-level-8 nil
					  :foreground chocolate-theme-white+2 :weight 'bold)
  (set-face-attribute 'org-block nil
					  :foreground chocolate-theme-white+1)
  (set-face-attribute 'org-done nil
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'org-todo nil
					  :foreground chocolate-theme-element)
  (set-face-attribute 'org-table nil
					  :foreground chocolate-theme-element+6)
  (set-face-attribute 'org-priority nil
					  :foreground chocolate-theme-element+10)
  (set-face-attribute 'org-drawer nil
					  :foreground nil
					  :inherit 'font-lock-comment-face)
  (set-face-attribute 'org-date nil
					  :foreground nil
					  :inherit 'font-lock-string-face)
  (set-face-attribute 'org-scheduled-today nil
					  :foreground chocolate-theme-white)
  (set-face-attribute 'org-scheduled nil
					  :foreground chocolate-theme-white)
  (set-face-attribute 'org-column nil
					  :background chocolate-theme-shadow+1
					  :foreground chocolate-theme-white+2)
  (set-face-attribute 'org-column-title nil
					  :background chocolate-theme-shadow+1
					  :foreground chocolate-theme-highlight+2)
  (with-eval-after-load 'org-habit
	(set-face-attribute 'org-habit-alert-face nil
						:foreground chocolate-theme-bg
						:background chocolate-theme-highlight+2)
	(set-face-attribute 'org-habit-alert-future-face nil
						:foreground chocolate-theme-bg
						:background chocolate-theme-highlight+3)
	(set-face-attribute 'org-habit-clear-face nil
						:foreground chocolate-theme-white
						:background chocolate-theme-shadow+3)
	(set-face-attribute 'org-habit-clear-future-face nil
						:foreground chocolate-theme-white
						:background chocolate-theme-shadow)
	(set-face-attribute 'org-habit-overdue-face nil
						:foreground chocolate-theme-bg
						:background chocolate-theme-highlight)
	(set-face-attribute 'org-habit-overdue-future-face nil
						:foreground chocolate-theme-bg
						:background chocolate-theme-highlight+1)
	(set-face-attribute 'org-habit-ready-face nil
						:foreground chocolate-theme-bg
						:background chocolate-theme-element)
	(set-face-attribute 'org-habit-ready-future-face nil
						:foreground chocolate-theme-bg
						:background chocolate-theme-element+1)))

(use-package org-capture
  :commands (org-capture)
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC ac" 'org-capture)

;;;###autoload
  (defun do-capture ()
	(interactive)
	(do-make-frame "capture")
	(org-capture))

  :config
  (general-define-key
   :keymaps 'org-capture-mode-map
   :states 'normal
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
	  (file+olp do--org-todo-location "Personal" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("P" "Personal TODO with reference" entry
	  (file+olp do--org-todo-location "Personal" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :REF:  %^{Reference}\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("a" "AUIS TODO" entry
	  (file+olp do--org-todo-location  "AUIS" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("A" "AUIS TODO with reference" entry
	  (file+olp do--org-todo-location  "AUIS" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :REF:  %^{Reference}\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("h" "Home TODO" entry
	  (file+olp do--org-todo-location  "Home" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("H" "Home TODO with reference" entry
	  (file+olp do--org-todo-location  "Home" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :REF:  %^{Reference}\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)
	 ("o" "Other TODO" entry
	  (file+olp do--org-todo-location  "Other" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :PROPERTIES:\n  :END:\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"
	  :kill-buffer t)))

  (define-advice org-capture (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '((display-buffer-same-window)
											  (inhibit-same-window . nil))))
	  (apply org-fn args))))

(use-package org-agenda
  :commands (org-agenda org-agenda-list)
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC aa" 'org-agenda)

;;;###autoload
  (defun do-agenda()
	"Called externally from the OS to launch Emacs and switch to org-agenda."
	(interactive)
	(org-agenda-list))

  :config
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states 'normal
   "<RET>" 'org-agenda-switch-to
   "l <RET>" '((lambda()(interactive)(do-make-frame)(org-agenda-switch-to))
			   :which-key "org-agenda-switch-new-frame")
   "<f5>" 'org-agenda
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
   "lk" 'org-agenda-kill
   "l#" 'org-agenda-set-tags
   "lu" 'org-agenda-add-note
   "la" 'org-attach
   "ls" 'org-agenda-schedule
   "lS" 'org-agenda-deadline
   "ld" 'org-agenda-date-prompt
   "lg" 'org-agenda-open-link
   "lt" 'nil
   "lta" 'org-agenda-clock-in
   "lti" 'org-agenda-clock-in
   "ltq" 'org-agenda-clock-out
   "ltx" 'org-agenda-clock-cancel
   "ltg" 'org-agenda-clock-goto
   "lx" 'org-agenda-archive)

  (setq org-agenda-skip-deadline-if-done t
		org-agenda-tags-column 0
		org-agenda-dim-blocked-tasks t
		org-agenda-weekend-days '(5 6)
		org-agenda-start-on-weekday 0
		org-agenda-use-time-grid t
		org-agenda-files (append
						  (file-expand-wildcards (concat do--org-files-location "*.org"))
						  (file-expand-wildcards (concat do--org-files-location  "archive/*.org"))))
 (add-to-list 'org-agenda-custom-commands
			 '("d" "Daily View"
			   ((tags-todo "urgent"
					  ((org-agenda-overriding-header "Urgent Tasks")))
			   (agenda "Today Overview"
					   ((org-agenda-start-day "0d")
						(org-agenda-span 2)
						(org-agenda-start-on-weekday 1)
						(org-agenda-start-with-log-mode '(closed))
						(org-agenda-overriding-header "Agenda")
						(org-agenda-skip-function '(lambda (&rest args)
													 (let ((result nil))
													   (setq result (org-agenda-skip-entry-if 'regexp "*\\* WAITING "))
													   (setq result (or result (org-agenda-skip-entry-if 'regexp "*\\* CANCELED ")))
													   (setq result (or result (org-agenda-skip-entry-if 'regexp "*\\* DONE "))))))))
			  (todo "WAITING"
					 ((org-agenda-start-with-log-mode '(closed))
					  (org-agenda-overriding-header "Waiting")
					  (org-agenda-files '("~/notes/todo.org"))))
			  (todo "DONE|CANCELED"
					 ((org-agenda-start-with-log-mode '(closed))
					  (org-agenda-overriding-header "Done")
					  (org-agenda-files '("~/notes/todo.org")))) )))

  (define-advice org-agenda (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '((display-buffer-same-window)
											  (inhibit-same-window . nil))))
	  (apply org-fn args)
	  ;; To forcefully enable text-scale-mode
	  (text-scale-set 0)))

  (define-advice org-agenda-date-prompt (:around (org-fn &rest args))
	(let ((display-buffer-overriding-action '(display-buffer-at-bottom))
		  (pop-up-frames 'nil))
	  (apply org-fn args)))

  (set-face-attribute 'org-agenda-done nil :foreground chocolate-theme-white+2)
  (set-face-attribute 'org-agenda-date-today nil
					  :foreground chocolate-theme-element+4)
  (set-face-attribute 'org-agenda-date nil
					  :foreground chocolate-theme-white+1)
  (set-face-attribute 'org-agenda-structure nil
					  :foreground chocolate-theme-highlight+2))

(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode)))

;; (use-package org-super-agenda
;;   :ensure t
;;   :hook ((org-mode . org-super-agenda-mode))
;;   :config
;;   (setq org-super-agenda-groups
;;		'((:name "Timeline" :time-grid t :todo "TODAY")
;;		  (:name "Important" :priority>= "B")
;;		  (:todo "WAITING" :order 8)
;;		  (:todo ("someday" "to-read" "check" "to-watch" "watching") :order 9)
;;		  (:name "Other" :priority<= "C" :order 1))))

;; (use-package ox-reveal)
;; (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
;; (setq org-reveal-mathjax t)
;; (use-package htmlize)
;; (use-package epresent)
;; (use-package org-tree-slide)

;; (use-package evil-org
;;	:ensure t
;;	:after (org)
;;	:hook ((org-mode . evil-org-mode))
;;	:config
;;	(add-hook 'evil-org-mode-hook
;;			  (lambda ()
;;				(evil-org-set-key-theme '(textobjects))
;;				(general-define-key
;;				 :keymaps 'evil-org-mode-map
;;				 :states '(normal)
;;				 "l" 'nil
;;				 "l" 'nil))))

(provide 'do-org)
