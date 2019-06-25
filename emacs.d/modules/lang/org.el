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

(cl-eval-when (compile)
  (require 'org)
  (require 'org-id)
  (require 'org-habit)
  (require 'org-agenda)
  (require 'org-capture))

(use-package org
  :defer t
  :init
  (general-define-key
   :keymaps 'override
   :prefix "SPC g"
   :states '(normal visual)
   ;; make a prefix-command and add description
   "" '(:ignore t :which-key "go")
   ;;shortcut to open my todo list
   "t" '(:ignore t :which-key "todo.org")
   "t" '(lambda()(interactive)(find-file "~/notes/todo.org")))

  :config
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   "<tab>" 'org-cycle
   "M-<tab>" 'org-cycle-level
   "xl" 'org-insert-link
   "xp" 'org-todo
   "x#" 'org-set-tags
   "M-c" 'org-metaup
   "M-t" 'org-metadown
   "M-h" 'org-metaleft
   "M-n" 'org-metaright)

  (general-define-key
   :keymaps 'org-mode-map
   :states 'normal
   "RET" 'org-cycle
   "M-<return>" 'org-global-cycle
   "C" 'org-shiftup
   "T" 'org-shiftdown
   "H" 'org-shiftleft
   "N" 'org-shiftright
   "M-C" 'org-shiftmetaup
   "M-T" 'org-shiftmetadown
   "M-H" 'org-shiftmetaleft
   "M-N" 'org-shiftmetaright
   "gC" 'org-up-element
   "gN" 'org-forward-element
   "gH" 'org-backward-element
   "gT" 'org-down-element
   "xt" '(:ignore t :which-key "time")
   "xtt" '(:ignore t :which-key "insert timestamp")
   "xtt" '(lambda () (interactive) (org-time-stamp (current-time)))
   "xt(" 'org-clock-in
   "xt)" 'org-clock-out
   "xtq" 'org-clock-cancel
   "xtg" 'org-clock-goto
   "xv" '(:ignore t :which-key "view")
   "xvl" 'org-toggle-latex-fragment
   "xs" 'org-schedule
   "xS" 'org-deadline
   "xu" 'org-add-note ;;progress
   "xa" 'org-attach
   "xg" 'org-open-at-point
   "xc" 'org-ctrl-c-ctrl-c
   "xi" 'org-id-get-create
   "x@" 'org-toggle-ordered-property
   "xe" '(:ignore t :which-key "export")
   "xel" 'org-export-dispatch
   "xep" 'org-latex-export-to-pdf
   "xeb" 'org-beamer-export-to-pdf
   "xeh" 'org-html-export-to-html
   "xq"  'org-ctrl-c-ctrl-c
   "x <escape>" 'org-kill-note-or-show-branches
   "x'" 'org-edit-src-code
   "xx" 'org-archive-subtree)

  (general-define-key
   :keymaps 'org-src-mode-map
   :states '(normal visual)
   "xq" 'org-edit-src-exit
   "x <escape>" 'org-edit-src-abort)

  (setq org-modules '(org-bbdb org-bibtex org-crypt org-docview
							   org-gnus org-habit org-id
							   org-info org-irc org-mhe
							   ;; org-rmail
							   ;; org-drill
							   org-w3m)
		org-id-locations-file "~/notes/org-id-location"
		org-id-track-globally t
		org-startup-with-inline-images t
		org-directory "~/notes"
		org-startup-indented nil
		org-startup-with-latex-preview t
		org-startup-folded "OVERVIEW"
		org-log-into-drawer "LOGBOOK"
		org-refile-allow-creating-parent-nodes 'confirm
		org-default-notes-file (concat org-directory "/todo.org")
		org-log-done 'time ;;every time we close a todo, org will add
		org-src-fontify-natively t
		;; Ordered tasks
		org-track-ordered-property-with-tag t ;; Add :ORDER:
		org-enforce-todo-checkbox-dependencies t ;; Don't allow the super task
												 ;; to close without completing
												 ;; all the sub checklists
		org-enforce-todo-dependencies t ;; Don't allow the super task to close
										;; without closing its sub tasks
		org-habit-graph-column 60)

  ;; (add-hook 'org-mode-hook
  ;;		  '(lambda ()
  ;;			 (with-eval-after-load 'company
  ;;			   (set (make-local-variable 'company-backends)
  ;;					(append  company-backends
  ;;							 '((company-ispell company-dabbrev)))))))

  ;; Theme
  (custom-set-variables
   '(org-format-latex-options
	 (list :foreground chocolate-theme-white :background chocolate-theme-bg
		   :scale 1.0 :html-foreground chocolate-theme-white
		   :html-background "Transparent"
		   :html-scale 1.0)))

  (set-face-attribute 'org-special-keyword nil
					  :inherit 'font-lock-comment-face)
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
					  :foreground chocolate-theme-element+4)
  (set-face-attribute 'org-todo nil
					  :foreground chocolate-theme-element)
  (set-face-attribute 'org-table nil
					  :foreground chocolate-theme-element+9)
  (set-face-attribute 'org-scheduled-today nil
					  :foreground chocolate-theme-white)
  (set-face-attribute 'org-scheduled nil
					  :foreground chocolate-theme-white))

(use-package org-capture
  :commands (org-capture)
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
  (general-define-key
   :keymaps 'org-capture-mode-map
   :states '(normal visual)
   "x <escape>" 'org-capture-kill
   "xq" 'org-capture-finalize)

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
	  (file+olp "~/notes/todo.org" "Personal" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n")
	 ("a" "AUIS TODO" entry
	  (file+olp "~/notes/todo.org" "AUIS" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n")
	 ("h" "Home TODO" entry
	  (file+olp "~/notes/todo.org" "Home" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n")
	 ("o" "Other TODO" entry
	  (file+olp "~/notes/todo.org" "Other" "Inbox")
	  "* TODO %?\n SCHEDULED:%^t\n  :LOGBOOK:\n  - Captured at %U\n  :END:\n"))))

(use-package org-agenda
  :commands (org-agenda org-agenda-list)
  :init
  (general-define-key
   :keymaps 'override
   :prefix "SPC g"
   :states '(normal visual)
   "a" 'org-agenda)

;;;###autoload
  (defun do-agenda()
	"Called externally from the OS to launch Emacs and switch to org-agenda."
	(interactive)
	(org-agenda-list))

  :config
  (general-def 'org-agenda-mode-map
	:states 'normal
	"RET" 'org-agenda-switch-to
	"q" 'org-agenda-quit
	;; Ordering
	"M-t" 'org-agenda-drag-line-forward
	"M-c" 'org-agenda-drag-line-backward
	"M-h" 'org-agenda-do-date-earlier
	"M-n" 'org-agenda-do-date-later
	;; Navigation
	"H" 'org-agenda-earlier
	"N" 'org-agenda-later
	;; Shift keys
	"C" 'org-agenda-priority-up
	"T" 'org-agenda-priority-down
	;; Go to
	"f" 'org-agenda-goto
	"gf" 'org-agenda-goto
	"gf" 'org-agenda-goto
	"gj" 'org-agenda-goto-date
	"gt" 'org-agenda-goto-today
	"F" 'org-agenda-follow-mode
	"j" 'org-agenda-goto-date
	;; View mode
	"v" 'nil
	"vd" 'org-agenda-day-view
	"vw" 'org-agenda-week-view
	"vm" 'org-agenda-month-view
	"vy" 'org-agenda-year-view
	"vf" 'org-agenda-fortnight-view
	"vD" 'org-agenda-toggle-diary
	"vl" 'org-agenda-log-mode
	"vc" 'org-agenda-clockreport-mode
	"vh" 'org-agenda-holidays
	"vS" 'org-agenda-toggle-deadlines
	"vq" 'org-agenda-reset-view
	;; Searching
	"/" 'nil
	"/#" 'org-agenda-filter-by-tag
	"//" 'evil-ex-search-forward
	"/?" 'evil-ex-search-backward
	"/%" 'org-agenda-filter-by-regexp
	"/c" 'org-agenda-filter-by-category
	"/h" 'org-agenda-filter-by-top-headline
	"/e" 'org-agenda-filter-by-effort
	"/q" 'org-agenda-filter-remove-all
	;; Extra actions
	"z" 'org-agenda-undo
	"C-z" 'org-agenda-redo
	"m" 'org-agenda-bulk-mark
	"M" 'org-agenda-bulk-mark-all
	"u" 'org-agenda-bulk-unmark
	"U" 'org-agenda-bulk-unmark-all
	"*" 'nil
	"**" 'org-agenda-bulk-mark-all
	"*t" 'org-agenda-bulk-toggle-all
	"*%" 'org-agenda-bulk-mark-regexp
	"*a" 'org-agenda-bulk-action
	"*A" 'org-agenda-bulk-action
	"xp" 'org-agenda-todo
	"xk" 'org-agenda-kill
	"x#" 'org-agenda-set-tags
	"xu" 'org-agenda-add-note
	"xa" 'org-attach
	"xs" 'org-agenda-schedule
	"xS" 'org-agenda-deadline
	"xd" 'org-agenda-date-prompt
	"xg" 'org-agenda-open-link
	"xt" 'nil
	"xta" 'org-agenda-clock-in
	"xti" 'org-agenda-clock-in
	"xtq" 'org-agenda-clock-out
	"xtx" 'org-agenda-clock-cancel
	"xtg" 'org-agenda-clock-goto
	"xx" 'org-agenda-archive)

  (setq org-agenda-skip-deadline-if-done t
		org-agenda-tags-column 0
		org-agenda-dim-blocked-tasks t
		org-agenda-weekend-days '(5 6)
		org-agenda-start-on-weekday 0
		org-agenda-use-time-grid nil
		;;a closing time stamp
		org-agenda-files (append
						  (file-expand-wildcards "~/notes/*.org")
						  (file-expand-wildcards "~/notes/archive/*.org")))

  ;; Open in a frame instead of a sub-window
  ;; Source: https://bit.ly/2WX44mj
  (define-advice org-agenda-list (:around (orig-fn &rest args))
  (cl-letf (((symbol-function 'switch-to-buffer-other-window)
			 (symbol-function 'switch-to-buffer-other-frame)))
	(apply orig-fn args)))

  (set-face-attribute 'org-agenda-done nil :foreground chocolate-theme-white+2)
  (set-face-attribute 'org-agenda-date-today nil
					  :foreground chocolate-theme-element+4)
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
;;				 "x" 'nil
;;				 "X" 'nil))))
