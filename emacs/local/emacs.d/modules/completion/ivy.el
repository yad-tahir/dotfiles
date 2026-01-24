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

  (general-define-key
   :map 'override
   :states '(normal visual)
   "SPC ;" 'ivy-resume)

  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "C-c C-k" 'nil
   "C-c" 'ivy-previous-line)

  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-o" 'nil
   "C-t" 'ivy-next-line
   "C-c" 'ivy-previous-line
   "C-h" 'left-char
   "C-n" 'right-char
   "M-c" 'ivy-beginning-of-buffer
   "M-t" 'ivy-end-of-buffer
   "C-." 'ivy-resume
   "<M-RET>" 'ivy-immediate-done
   "M-p" 'ivy-yank-word
   "C-p" 'yank
   "C-w" 'forward-word
   "C-b" 'backward-word
   "C-$" 'move-end-of-line
   "C-a" 'move-end-of-line
   "C-0" 'move-beginning-of-line
   "C-i" 'move-beginning-of-line
   "C-g" 'minibuffer-keyboard-quit
   "C-q" 'minibuffer-keyboard-quit
   "M-<backspace>" 'do--ivy-reset
   "<tab>" 'do--ivy-tab
   "M-<tab>" 'ivy-next-line-and-call
   "C-l" 'ivy-read-action
   "C-u" 'kill-line
   "C-d" 'kill-whole-line
   "C-k" 'do--ivy-kill
   "<escape>" 'do-evil-escape-abort)

  (general-def
    [remap switch-to-buffer] 'ivy-switch-buffer
    [remap ibuffer] 'ivy-switch-buffer
    [remap imenu-anywhere] 'ivy-imenu-anywhere)

  ;; To support TAB in both GUI and Terminal
  (unless (display-graphic-p)
    ;; No ideal as it overshadow 'C-i'; Terminals send the exact same signal
    ;; (ASCII code 9) for both the TAB key and Ctrl+i
    (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial))

  (setq ivy-height 20
        ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-fixed-height-minibuffer t)

  ;; Ignore case sensitivity
  (setq-default completion-ignore-case t
                read-file-name-completion-ignore-case t
                read-buffer-completion-ignore-case t
                ivy-case-fold-search-default 'always
                ivy-re-builders-alist
                '((t . ivy--regex-ignore-order)))

  (defun do--ivy-reset ()
    "Reset ivy prompt."
    (interactive)
    (if (minibuffer-window-active-p (minibuffer-window))
        (progn
          (setq ivy-exit 'done) ;; Tell Ivy we are done
          (exit-minibuffer))
      (keyboard-quit)))

  (defun do--ivy-tab ()
    "Tab operation based on ivy minibuffer type."
    (interactive)
    (let ((caller (when (bound-and-true-p ivy-last)
                    (ivy-state-caller ivy-last))))
      (if (member caller '(counsel-rg counsel-ag counsel-grep))
          (ivy-next-line-and-call)
        (ivy-partial))))

  (defun do--ivy-kill ()
    "Delete operation based on ivy minibuffer type."
    (interactive)
    (if (eq (ivy-state-caller ivy-last) 'counsel-bookmark)
        ;; Let me select delete via menu; not ideal but it is the easiest
        ;; way to refresh ivy
        (when-let ((curr (ivy-state-current ivy-last)))
          (ivy-read-action))
      ;; Fallback for other Ivy commands
      (ivy-kill-line)))

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
                      counsel-yank-pop counsel-describe-face
                      do-counsel-rg-project-at-point)
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
   "SPC sf" '(lambda ()
               (interactive)
               (require 'projectile)
               (let ((counsel-fzf-cmd "rg --color never --files -g '*%s*'")
                     (root-dir (or (projectile-project-root) default-directory)))
                 (counsel-fzf nil root-dir "File ")))
   "SPC sF" '((lambda ()
                (interactive)
                (require 'projectile)
                (let ((counsel-fzf-cmd "rg --color never -u --files -g '*%s*'")
                      (root-dir (or (projectile-project-root) default-directory)))
                  (counsel-fzf nil root-dir "File (including hidden) " )))
              :which-key "hidden-file")
   "SPC sj" 'counsel-dired-jump
   "SPC sv" 'counsel-describe-variable
   "SPC sl" 'counsel-find-library
   "SPC si" 'counsel-info-lookup-symbol
   "SPC sg" 'do-counsel-rg-project-at-point)

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
        ;; counsel-rg-base-command "rg -S --hidden --no-binary --color never --no-heading %s"
        ;; Configure counsel-fzf to use rg instead.
        counsel-fzf-dir-function 'counsel-fzf-dir-function-projectile
        counsel-fzf-cmd "rg --color never -u --files -g '*%s*' ")

  (defun do-counsel-rg-project-at-point ()
    "Ask to search for word at point.
   y       -> Search for the word immediately.
   n / RET -> Open empty search (default)."
    (interactive)
    (let* ((root (projectile-project-root))
           (word (thing-at-point 'symbol t))
           (search-input nil))

      (if word
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map minibuffer-local-map)
            (define-key map (kbd "y")
                        (lambda () (interactive)
                          (insert "y")
                          (exit-minibuffer)))
            (define-key map (kbd "n") 'exit-minibuffer)
            (define-key map (kbd "RET") 'exit-minibuffer)
            (define-key map (kbd "<return>") 'exit-minibuffer)

            (let ((choice (read-from-minibuffer
                           (format "Search for '%s'? [y/N] " word)
                           nil map)))
              (when (string= choice "y")
                (setq search-input word)))))

      (counsel-rg search-input root)))

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
             counsel-projectile-switch-project
             counsel-projectile-rg)
  :init
  ;; Keybindings
  (general-def ivy-mode-map
    [remap projectile-find-file]     #'counsel-projectile-find-file
    [remap projectile-find-dir]      #'counsel-projectile-find-dir
    [remap counsel-rg]               #'counsel-projectile-rg)

  :config
  (require 'projectile)
  (projectile-mode 1)
  ;; Use git's search engine. This enables occur-mode on counsel-find-file
  (ivy-set-occur 'counsel-projectile-find-file 'counsel-git-occur))

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
