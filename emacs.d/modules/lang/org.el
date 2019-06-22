;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019

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

(with-no-warnings
  (use-package org
	:ensure t
	:defer t
	:preface
	(declare-function do--org-indent nil)

	:init
	(general-define-key
	 :keymaps 'override
	 :prefix "SPC g"
	 :states '(normal visual)
	 ;; make a prefix-command and add description
	 "" #'(:ignore t :which-key "go")
	 ;;shortcut to open my todo list
	 "t" #'(lambda()(interactive)(find-file "~/notes/todo.org")))

	:config
	(general-define-key
	 :keymaps 'org-mode-map
	 :states '(normal visual)
	 "<tab>" #'org-cycle
	 "M-<tab>" #'org-cycle-level
	 "ll" #'org-insert-link
	 "lp" #'org-todo
	 "l#" #'org-set-tags
	 "M-c" #'org-metaup
	 "M-t" #'org-metadown
	 "M-h" #'org-metaleft
	 "M-n" #'org-metaright)

	(general-def 'org-mode-map
	  :states 'normal
	  "RET" #'org-cycle
	  "M-<return>" #'org-global-cycle
	  "C" #'org-shiftup
	  "T" #'org-shiftdown
	  "H" #'org-shiftleft
	  "N" #'org-shiftright
	  "M-C" #'org-shiftmetaup
	  "M-T" #'org-shiftmetadown
	  "M-H" #'org-shiftmetaleft
	  "M-N" #'org-shiftmetaright
	  "=" #'do--org-indent
	  "gC"	#'org-up-element
	  "gN"	#'org-forward-element
	  "gH"	#'org-backward-element
	  "gT"	#'org-down-element
	  "lt" '(:ignore t :which-key "time")
	  "ltt" '(lambda () (interactive) (org-time-stamp (current-time)))
	  "lti" #'org-clock-in
	  "lta" #'org-clock-in
	  "ltq" #'org-clock-out
	  "ltx" #'org-clock-cancel
	  "ltg" #'org-clock-goto
	  "lv" '(:ignore t :which-key "view")
	  "lvl" #'org-toggle-latex-fragment
	  "lvm" #'org-toggle-latex-fragment
	  "ls" #'org-schedule
	  "lS" #'org-deadline
	  "lu" #'org-add-note ;;progress
	  "la" #'org-attach
	  "lg" #'org-open-at-point
	  "lc" #'org-ctrl-c-ctrl-c
	  "li" #'org-id-get-create
	  "l@" #'org-toggle-ordered-property
	  "le" '(:ignore t :which-key "export")
	  "lel" #'org-export-dispatch
	  "lep" #'org-latex-export-to-pdf
	  "leb" #'org-beamer-export-to-pdf
	  "leh" #'org-html-export-to-html
	  "lq" #'org-kill-note-or-show-branches
	  "l'" #'(lambda ()(interactive)(do-make-frame)(org-edit-src-code))
	  "lx" #'org-archive-subtree)

	(general-def 'org-mode-map
	  :states 'insert
	  "RET" #'org-return
	  "M-<return>" #'evil-open-below)

	(general-def 'org-src-mode-map
	  :states 'normal
	  "lq" '(lambda ()(interactive) (org-edit-src-exit)(evil-quit))
	  "l <escape>" #'(lambda ()(interactive) (org-edit-src-abort)(evil-quit)))

	(setq org-modules '(org-bbdb org-bibtex org-crypt org-docview
								 org-gnus org-habit org-id org-info org-irc org-mhe
								 ;; org-rmail
								 ;; org-drill
								 org-w3m)
		  org-id-locations-file "~/notes/org-id-location"
		  org-clock-into-drawer "CLOCKING"
		  org-directory "~/notes"
		  org-startup-indented nil
		  org-id-track-globally t
		  org-startup-folded "OVERVIEW"
		  org-startup-with-latex-preview t
		  org-log-into-drawer "LOGBOOK"
		  org-refile-allow-creating-parent-nodes 'confirm
		  org-default-notes-file (concat org-directory "/todo.org")
		  org-log-done 'time ;;every time we close a todo, org will add
		  org-src-fontify-natively t
		  ;; Ordered tasks
		  org-track-ordered-property-with-tag t ;; Add :ORDER: tag to the ordered tasks
		  org-enforce-todo-checkbox-dependencies t ;; Don't allow the super task to close without completing all the sub checklists
		  org-enforce-todo-dependencies t ;; Don't allow the super task to close without closing its sub tasks
		  org-habit-graph-column 60)

	;; Auto generate org ids when the file is saved
	;;   (defun my/org-add-ids-to-headlines-in-file ()
	;;	"Add ID properties to all headlines in the current file which
	;; do not already have one."

	;;	(interactive)
	;;	(org-map-entries 'org-id-get-create))

	;; (add-hook 'org-mode-hook
	;;		  '(lambda ()
	;;			 (with-eval-after-load 'company
	;;			   (set (make-local-variable 'company-backends)
	;;					(append  company-backends
	;;							 '((company-ispell company-dabbrev)))))))

	(custom-set-variables
	 '(org-format-latex-options
	   (list :foreground chocolate-theme-white :background chocolate-theme-bg
			 :scale 1.0 :html-foreground chocolate-theme-white
			 :html-background "Transparent"
			 :html-scale 1.0)))

	(set-face-attribute 'org-special-keyword nil :inherit 'font-lock-comment-face)
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
	(set-face-attribute 'org-block nil :foreground chocolate-theme-white+1)
	(set-face-attribute 'org-done nil :foreground chocolate-theme-element+4)
	(set-face-attribute 'org-todo nil :foreground chocolate-theme-element)
	(set-face-attribute 'org-table nil :foreground chocolate-theme-element+9)
	(set-face-attribute 'org-scheduled-today nil :foreground chocolate-theme-white)
	(set-face-attribute 'org-scheduled nil :foreground chocolate-theme-white)

	(defun do--org-indent ()
	  (interactive)
	  (save-excursion
		(org-set-tags t t) ;; align tags if it is needed
		(call-interactively 'evil-indent)))

	(defun evil-org-eol-call (fun)
	  "Go to end of line and call provided function.
FUN function callback"
	  (end-of-line)
	  (funcall fun)
	  (evil-append 0)))

  (use-package org-agenda
	:after (org)
	:defer t
	:commands (org-agenda)
	:init
	(general-define-key
	 :keymaps 'override
	 :prefix "SPC g"
	 :states '(normal visual)
	 "a" #'org-agenda)

;;;###autoload
	(defun do-agenda()
	  (interactive)
	  (do-make-frame "agenda")
	  (org-agenda-list))

	:config
	(general-def 'org-agenda-mode-map
	  :states 'normal
	  "RET" #'org-agenda-switch-to
	  "q" #'org-agenda-quit
	  ;; Ordering
	  "M-t" #'org-agenda-drag-line-forward
	  "M-c" #'org-agenda-drag-line-backward
	  "M-h" #'org-agenda-do-date-earlier
	  "M-n" #'org-agenda-do-date-later
	  ;; Navigation
	  "H" #'org-agenda-earlier
	  "N" #'org-agenda-later
	  ;; Shift keys
	  "C" #'org-agenda-priority-up
	  "T" #'org-agenda-priority-down
	  ;; Go to
	  "f" #'org-agenda-goto
	  "gf" #'org-agenda-goto
	  "gf" #'org-agenda-goto
	  "gj" #'org-agenda-goto-date
	  "gt" #'org-agenda-goto-today
	  "F" #'org-agenda-follow-mode
	  "j" #'org-agenda-goto-date
	  ;; View mode
	  "v" 'nil
	  "vd" #'org-agenda-day-view
	  "vw" #'org-agenda-week-view
	  "vm" #'org-agenda-month-view
	  "vy" #'org-agenda-year-view
	  "vf" #'org-agenda-fortnight-view
	  "vD" #'org-agenda-toggle-diary
	  "vl" #'org-agenda-log-mode
	  "vc" #'org-agenda-clockreport-mode
	  "vh" #'org-agenda-holidays
	  "vS" #'org-agenda-toggle-deadlines
	  "vq" #'org-agenda-reset-view
	  ;; Searching
	  "/" 'nil
	  "/#" #'org-agenda-filter-by-tag
	  "//" #'evil-ex-search-forward
	  "/?" #'evil-ex-search-backward
	  "/%" #'org-agenda-filter-by-regexp
	  "/c" #'org-agenda-filter-by-category
	  "/h" #'org-agenda-filter-by-top-headline
	  "/e" #'org-agenda-filter-by-effort
	  "/q" #'org-agenda-filter-remove-all
	  ;; Extra actions
	  "z" #'org-agenda-undo
	  "C-z" #'org-agenda-redo
	  "m" #'org-agenda-bulk-mark
	  "M" #'org-agenda-bulk-mark-all
	  "u" #'org-agenda-bulk-unmark
	  "U" #'org-agenda-bulk-unmark-all
	  "*" 'nil
	  "**" #'org-agenda-bulk-mark-all
	  "*t" #'org-agenda-bulk-toggle-all
	  "*%" #'org-agenda-bulk-mark-regexp
	  "*a" #'org-agenda-bulk-action
	  "*A" #'org-agenda-bulk-action
	  "lp" #'org-agenda-todo
	  "lk" #'org-agenda-kill
	  "l#" #'org-agenda-set-tags
	  "lu" #'org-agenda-add-note
	  "la" #'org-attach
	  "ls" #'org-agenda-schedule
	  "lS" #'org-agenda-deadline
	  "ld" #'org-agenda-date-prompt
	  "lg" #'org-agenda-open-link
	  "lt" 'nil
	  "lta" #'org-agenda-clock-in
	  "lti" #'org-agenda-clock-in
	  "ltq" #'org-agenda-clock-out
	  "ltx" #'org-agenda-clock-cancel
	  "ltg" #'org-agenda-clock-goto
	  "lx" #'org-agenda-archive)

	(setq org-agenda-skip-deadline-if-done t
		  org-agenda-tags-column 0
		  org-agenda-dim-blocked-tasks t
		  org-agenda-weekend-days '(5 6)
		  org-agenda-start-on-weekday 0
		  ;;a closing time stamp
		  org-agenda-files (append
							(file-expand-wildcards "~/notes/*.org")
							(file-expand-wildcards "~/notes/archive/*.org")))

	(set-face-attribute 'org-agenda-done nil :foreground chocolate-theme-white+2)
	(set-face-attribute 'org-agenda-date-today nil :foreground chocolate-theme-element+4)
	(set-face-attribute 'org-agenda-structure nil :foreground chocolate-theme-highlight+2))

  (use-package org-capture
	:after (org)
	:commands (org-capture)
	:defer t
	:init
	(general-define-key
	 :keymaps 'override
	 :prefix "SPC g"
	 :states '(normal visual)
	 "c" #'org-capture)

;;;###autoload
	(defun do-capture ()
	  (interactive)
	  (do-make-frame "capture")
	  (org-capture))

	:config
	(general-def 'org-capture-mode-map
	  :states '(normal visual)
	  "lq" #'org-capture-kill
	  "lc" #'org-capture-finalize)

	(setq

	 ;; %? the initial position of the cursor
	 ;; %^g prompt for tags
	 ;; %^t prompt for a date
	 ;; %^L prompt for a link
	 ;; %i place the selected text in the other window
	 ;; %^{something} prompt for string
	 ;; ;; Further details can be found at https://orgmode.org/manual/Template-expansion.html#Template-expansion
	 org-capture-templates '(("p" "Personal TODO" entry
							  (file+olp "~/notes/todo.org" "Personal" "Inbox")
							  "* TODO %? %^g\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:%^{REFERENCE}p\n  %i \n" )
							 ("a" "AUIS TODO" entry
							  (file+olp "~/notes/todo.org" "AUIS" "Inbox")
							  "* TODO %? %^g\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:%^{REFERENCE}p\n  %i \n" )
							 ("h" "Home TODO" entry
							  (file+olp "~/notes/todo.org" "Home" "Inbox")
							  "* TODO %? %^g\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n  %i \n" )
							 ("o" "Other TODO" entry
							  (file+olp "~/notes/todo.org" "Other" "Inbox")
							  "* TODO %? %^g\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n  %i \n" ))))

  (use-package org-bullets
	:after (org)
	:ensure t
	:hook ((org-mode . org-bullets-mode)))

  ;;   (use-package org-super-agenda
  ;;	:hook ((org-mode . org-super-agenda-mode))
  ;;	:config
  ;;	(setq org-super-agenda-groups
  ;;		  '((:name "Timeline" :time-grid t :todo "TODAY")
  ;;			(:name "Important" :priority>= "B")
  ;;			(:todo "WAITING" :order 8)
  ;;			(:todo ("someday" "to-read" "check" "to-watch" "watching") :order 9)
  ;;			(:name "Other" :priority<= "C" :order 1))))

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
  ;;				 "x" 'nil
  ;;				 "X" 'nil))))
  )
