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

(use-package ivy
  :ensure t
  :after (evil)
  :config
  ;; Remove compile warnings
  (declare-function ivy-set-actions nil)
  (declare-function ivy-set-occur nil)

  ;; Keybinding
  (general-define-key
   :keymaps 'evil-ex-completion-map
   "C-." 'ivy-resume)

  (general-define-key
   :keymaps 'ivy-mode-map
   [remap ibuffer] #'ivy-switch-buffer
   [remap switch-to-buffer] #'ivy-switch-buffer)

  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "C-c C-k" 'nil
   "C-c" 'ivy-previous-line
   "C-q" 'ivy-switch-buffer-kill)

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   ;; Navigation
   "C-t" #'ivy-next-line
   "C-c" 'ivy-previous-line
   "C-h" 'left-char
   "C-n" 'right-char
   "M-c" 'ivy-beginning-of-buffer
   "M-t" 'ivy-end-of-buffer
   ;; Special commands
   ;; "C-e" 'ivy-occur
   "C-." 'ivy-resume
   ;; "C-M-i" 'ivy-next-line-and-call
   "<M-RET>" 'ivy-immediate-done
   ;; Other commons
   "M-p" 'ivy-yank-word
   "C-p" 'yank
   "C-w" 'forward-word
   "C-b" 'backward-word
   "C-$" 'move-end-of-line
   "C-0" 'move-beginning-of-line
   "C-i" 'backward-kill-word
   "C-a" 'kill-word
   "C-u" 'kill-line
   "C-d" 'kill-whole-line
   "<escape>" 'do-evil-escape-abort
   "C-k" 'describe-key)

   ;; To support TAB in both GUI and Terminal
   (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)

  (general-define-key
   :keymaps '(ivy-occur-mode-map ivy-occur-grep-mode-map)
   :states 'normal
   "<RET>" 'ivy-occur-press
   "TAB" 'ivy-occur-read-action
   "}" 'next-line
   "{" 'previous-line)

  (defalias 'ibuffer 'ivy-switch-buffer)
  (defalias 'imenu-anywhere 'ivy-imenu-anywhere)

  ;; Basic settings
  (setq ivy-height 15
		ivy-use-virtual-buffers t
		enable-recursive-minibuffers t
		ivy-wrap t
		ivy-fixed-height-minibuffer t)

  (ivy-mode))

(use-package counsel
  :ensure t
  :commands
  (counsel-dired-jump counsel-git-grep counsel-describe-variable
					  counsel-find-library counsel-describe-function
					  counsel-info-lookup-symbol
					  counsel-unicode-char counsel-semantic-or-imenu
					  counsel-fzf counsel-ag counsel-apropos counsel-rg
					  counsel-describe-face counsel-find-file
					  counsel-recentf counsel-imenu counsel-bookmark
					  counsel-M-x counsel-org-capture
					  counsel-yank-pop counsel-describe-face)
  :preface
  (declare-function counsel-cmd-to-dired nil)
  :init
  ;; Keybindings
  (general-define-key
   :states '(normal visual)
   ;; make a prefix-command and add description
   "g@" #'counsel-semantic-or-imenu
   "M-p" #'counsel-yank-pop)

  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "M-p" 'counsel-yank-pop
   ;; make a prefix-command and add description
   "SPC s" '(:ignore t :which-key "search")
   "SPC sf" '((lambda ()
				(interactive)
				(let ((counsel-fzf-cmd "rg --color never --files -g '*%s*'"))
				  (counsel-fzf nil nil "file " )))
			 :which-key "file")
   "SPC sF" '((lambda ()
				(interactive)
				(let ((counsel-fzf-cmd "rg --color never -u --files -g '*%s*'"))
				  (counsel-fzf nil nil "file " )))
			  :which-key "hidden-file")
   "SPC sj" 'counsel-dired-jump
   "SPC sv" 'counsel-describe-variable
   "SPC sl" 'counsel-find-library
   "SPC si" 'counsel-info-lookup-symbol
   "SPC sG" 'counsel-rg)

  (general-define-key
   :keymaps 'ivy-mode-map
   [remap apropos]                   #'counsel-apropos
   [remap find-file]                 #'counsel-find-file
   [remap recentf-open-files]        #'counsel-recentf
   [remap imenu]                     #'counsel-imenu
   [remap bookmark-jump]             #'counsel-bookmark
   [remap execute-extended-command]  #'counsel-M-x
   [remap org-capture]               #'counsel-org-capture
   [remap describe-face]             #'counsel-describe-face)

  :config
  ;; Start projectile since we need it for counsel-ag and counsel-rg commands
  (require 'projectile)
  (projectile-mode 1)

  (setq counsel-switch-buffer-preview-virtual-buffers t
		;;counsel-ag-base-command "ag --hidden --nocolor --nogroup %s"
		counsel-rg-base-command "rg -S --no-binary --no-heading --line-number -uu --color never %s "
		;; Configure counsel-fzf to use rg instead.
		counsel-fzf-dir-function 'counsel-fzf-dir-function-projectile
		counsel-fzf-cmd "rg --color never -u --files -g '*%s*' ")

	;;; Enable rg export by ignoring counsel's fzf internal processing.
  (defun do--counsel-fzf-occur (&rest args)
	"Occur function for `counsel-fzf' to use 'ag' instead "
	(ignore args)
	(cd counsel--fzf-dir)
	(counsel-cmd-to-dired
	 (concat
	  (format counsel-fzf-cmd ivy-text)
	  ;; The sed is required to change ' to \'. Otherwise, xargs will throw
	  ;; exceptions when file names contain single quotes.
	  "| sed -e \"s/'/\\\\\\\\'/g\" | xargs -I {} ls -alih ./{}")))
  (ivy-set-occur 'counsel-fzf 'do--counsel-fzf-occur))

(use-package counsel-projectile
  :ensure t
  :commands (counsel-projectile-find-dir
			 counsel-projectile-find-file
			 counsel-projectile-switch-project)
  :init
  ;; Keybindings
  (general-def ivy-mode-map
	[remap projectile-find-file]      #'counsel-projectile-find-file
	[remap projectile-find-dir]      #'counsel-projectile-find-dir)

  :config
  (require 'projectile)
  (projectile-mode 1)
  ;; Use git's search engine. This enables occur-mode on counsel-find-file
  (ivy-set-occur 'counsel-projectile-find-file 'counsel-git-occur))

(use-package swiper
  :commands (swiper)
  :init
  (general-define-key
   :states '(visual normal)
   "SPC l/" 'swiper)
  :config
  (general-define-key
   :keymaps 'swiper-map
   "C-t" 'ivy-next-line
   "C-c C-f" 'nil
   "C-c" 'ivy-previous-line
   "M-c" 'ivy-beginning-of-buffer
   "M-t" 'ivy-end-of-buffer
   "C-w" 'forward-word
   "C-b" 'backward-word
   "C-$" 'move-end-of-line
   "C-0" 'move-beginning-of-line
   "C-q" 'ivy-immediate-done))

;; (use-package wgrep
;;   :ensure t
;;   :disabled t
;;   :commands (wgrep-change-to-wgrep-mode)
;;   :init
;;   (general-define-key
;;    :keymaps 'ivy-occur-grep-mode-map
;;    :states 'normal
;;    "l" 'nil
;;    "lw" 'wgrep-change-to-wgrep-mode)

;;   :config
;;   (general-define-key
;;    :keymaps 'wgrep-mode-map
;;    :states 'normal
;;    "SPC lw" 'wgrep-finish-edit
;;    "l <escape>" 'wgrep-abort-changes
;;    "lz" 'wgrep-remove-all-change)

;;   (setq wgrep-auto-save-buffer t))

(provide 'do-ivy)
