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

(use-package ivy
  :demand t
  :ensure t
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
   "C-t" 'ivy-next-line
   "C-c" 'ivy-previous-line
   "C-h" 'left-char
   "C-n" 'right-char
   "M-c" 'ivy-beginning-of-buffer
   "M-t" 'ivy-end-of-buffer
   ;; Special commands
   "C-e" 'ivy-occur
   "C-." 'ivy-resume
   "C-M-i" 'ivy-next-line-and-call
   "<M-RET>" 'ivy-immediate-done
   ;; Other commons
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

  ;; Theme
  (set-face-attribute 'ivy-cursor nil
					  :inherit 'cursor
					  :foreground nil
					  :background chocolate-theme-bg)
  (set-face-attribute 'ivy-current-match nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-white :weight 'bold)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-white :weight 'bold)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-white :weight 'bold)
  (set-face-attribute 'ivy-modified-buffer nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-highlight+3)
  (set-face-attribute 'ivy-virtual nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'ivy-modified-outside-buffer nil
					  :inherit nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-highlight :weight 'bold)
  (set-face-attribute 'ivy-org nil
					  :inherit nil
					  :background nil
					  :foreground chocolate-theme-white+1
					  :weight 'bold)

  ;; Make ivy identical to some of the faces in dired mode. However,
  ;; we don't want to inherit from dired as this might trigger auto-loading
  (set-face-attribute 'ivy-subdir nil :inherit 'font-lock-function-name-face
					  :background nil
					  :foreground nil)
  (set-face-attribute 'ivy-remote nil :inherit 'font-lock-keyword-face
					  :background nil :foreground nil)

  (ivy-mode))


;;;
;;; Third-party packages
;;;

(use-package counsel
  :ensure t
  :defer 5
  ;; @SPEED: Un-comment to improve startup time
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
   "gp" #'counsel-yank-pop)

  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "M-p" 'counsel-yank-pop
   ;; make a prefix-command and add description
   "SPC s" '(:ignore t :which-key "search")
   "SPC sf" '(lambda () (interactive) (counsel-fzf nil nil "file " ))
   "SPC sj" 'counsel-dired-jump
   "SPC sv" 'counsel-describe-variable
   "SPC sl" 'counsel-find-library
   "SPC sSPC l" 'counsel-describe-function
   "SPC si" 'counsel-info-lookup-symbol
   "SPC su" 'counsel-unicode-char
   "SPC s'" 'counsel-bookmark
   "SPC sg" 'counsel-rg)

  (general-define-key
   :keymaps 'ivy-mode-map
   [remap apropos]                   #'counsel-apropos
   [remap describe-face]             #'counsel-describe-face
   [remap find-file]                 #'counsel-find-file
   [remap recentf-open-files]        #'counsel-recentf
   [remap imenu]                     #'counsel-imenu
   [remap bookmark-jump]             #'counsel-bookmark
   [remap execute-extended-command]  #'counsel-M-x
   [remap org-capture]               #'counsel-org-capture
   [remap describe-face]             #'counsel-describe-face)

  :config
  ;; It is useful to have a delete option on files in counsel.
  ;; Although I don't use this option that much.
  (ivy-set-actions 'counsel-find-file '(("d" delete-file "delete")))

  ;; Start projectile since we need it for counsel-ag and counsel-rg commands
  (require 'projectile)
  (projectile-mode 1)

  (setq counsel-ag-base-command "ag --hidden --nocolor --nogroup %s"
		counsel-rg-base-command "rg -S --no-binary --no-heading --line-number -uu --color never %s "
		;; Configure counsel-fzf to use rg instead.
		counsel-fzf-dir-function 'counsel-fzf-dir-function-projectile
		counsel-fzf-cmd "rg --color never -uu --files -g '*%s*' ")

	;;; Enable rg export by ignoring counsel's fzf internal processing.
  (defun do--counsel-fzf-occur ()
	"Occur function for `counsel-fzf' to use 'ag' instead "
	(cd counsel--fzf-dir)
	(counsel-cmd-to-dired
	 (concat
	  (format counsel-fzf-cmd ivy-text)
	  ;; The sed is required to change ' to \'. Otherwise, xargs will throw
	  ;; exceptions when file names contain single quotes.
	  "| sed -e \"s/'/\\\\\\\\'/g\" | xargs -I {} ls -l ./{}")))
  (ivy-set-occur 'counsel-fzf 'do--counsel-fzf-occur))


(use-package counsel-projectile
  :commands (counsel-projectile-find-dir
			 counsel-projectile-find-file
			 counsel-projectile-switch-project)
  :ensure t
  :init
  ;; Keybindings
  (general-def ivy-mode-map
	[remap projectile-switch-project] #'counsel-projectile-switch-project
	[remap projectile-find-file]      #'counsel-projectile-find-file)

  (general-define-key
   :prefix "SPC s"
   :keymaps 'override
   :states '(normal visual)
   ;; make a prefix-command and add description
   "" '(:ignore t :which-key "search")
   "F" 'counsel-projectile-find-file
   "D" 'counsel-projectile-find-dir)

  :config
  (require 'projectile)
  (projectile-mode 1)
  ;; Use git's search engine. This enables occur-mode on counsel-find-file
  (ivy-set-occur 'counsel-projectile-find-file 'counsel-git-occur))


(use-package swiper
  :disabled t
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

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode)
  :init
  (general-define-key
   :keymaps 'ivy-occur-grep-mode-map
   :states 'normal
   "l" 'nil
   "lw" 'wgrep-change-to-wgrep-mode)

  :config
  (general-define-key
   :keymaps 'wgrep-mode-map
   :states 'normal
   [remap evil-save-and-quit] 'wgrep-finish-edit
   "l <escape>" 'wgrep-abort-changes
   "lz" 'wgrep-remove-all-change)

  (setq wgrep-auto-save-buffer t))


(provide 'do-ivy)
