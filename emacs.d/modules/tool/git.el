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

(use-package magit
  :defer 20
  :ensure t
  :functions evil-set-initial-state
  :commands (magit-status magit-file-dispatch magit-dispatch)

  :init
  (general-define-key
   :prefix "SPC g"
   :keymaps 'override
   :states 'normal
   ;; make a prefix-command and add description
   "" '(:ignore t :which-key "go")
   "g" #'magit-status)

  :config
  ;; Remove conflicting key bindings
  (general-define-key
   :keymaps '(magit-file-section-map
			  magit-hunk-section-map
			  magit-log-mode-map)
   "C" 'nil
   "C-c" 'nil)
  ;; Remove all keybindings in the blob mode to avoid future bugs
  (setf (cdr magit-blob-mode-map) nil)

  (general-define-key
   :keymaps 'magit-log-select-mode-map
   :states '(normal visual)
   "lq" 'magit-log-select-pick
   "l <escape>" 'magit-log-select-quit)

  (general-define-key
   :keymaps 'magit-log-mode-map
   :states 'normal
   "p"  'magit-log-move-to-parent
   "s"  'magit-log-toggle-commit-limit
   "+"  'magit-log-double-commit-limit
   "-"  'magit-log-half-commit-limit
   "r"  'magit-rebase
   "m"  'magit-merge
   "x"  'magit-reset
   "l"  'nil
   "lp" 'magit-log-move-to-parent
   "ls" 'magit-log-toggle-commit-limit
   "lr" 'magit-rebase
   "lm"  'magit-merge
   "lx"  'magit-reset
   "l+" 'magit-log-double-commit-limit
   "l-" 'magit-log-half-commit-limit)

  (general-define-key
   :keymaps 'git-rebase-mode-map
   :states 'normal
   "<return>" 'git-rebase-show-commit
   "p"        'git-rebase-pick
   "d"        'git-rebase-kill-line
   "j"        'git-rebase-break
   "e"        'git-rebase-edit
   "b"        'git-rebase-label
   "m"        'git-rebase-merge
   "f"        'git-rebase-fixup
   "w"        'git-rebase-reword
   "s"        'git-rebase-squash
   "r"        'git-rebase-reset
   "!"        'git-rebase-exec
   "i"        'git-rebase-insert
   "x"        'git-rebase-noop
   "lp"       'git-rebase-pick
   "l,"       'git-rebase-kill-line
   "lb"       'git-rebase-break
   "le"       'git-rebase-edit
   "ll"       'git-rebase-label
   "lm"       'git-rebase-merge
   "lM"       'git-rebase-merge-toggle-editmsg
   "lf"       'git-rebase-fixup
   "lw"       'git-rebase-reword
   "ls"       'git-rebase-squash
   "lt"       'git-rebase-reset
   "lx"       'git-rebase-exec
   "li"       'git-rebase-insert
   "lz"       'git-rebase-noop
   "<space>"  'git-rebase-show-or-scroll-up
   "DEL"      'git-rebase-show-or-scroll-down
   "M-c"      'git-rebase-move-line-up
   "M-t"      'git-rebase-move-line-down
   "z"        'git-rebase-undo)

  (general-define-key
   :keymaps '(magit-file-section-map
			  magit-untracked-section-map
			  magit-unstaged-section-map
			  magit-unmerged-section-map
			  magit-merge-preview-mode-map
			  magit-staged-section-map
			  magit-hunk-section-map)
   [return] 'magit-visit-thing
   [C-return] 'magit-jump-to-diffstat-or-diff
   "a" 'magit-apply
   "s" 'magit-stage
   "u" 'magit-unstage
   "k" 'magit-discard
   "i" 'magit-commit-add-log
   "la" 'magit-apply
   "ls" 'magit-stage
   "lu" 'magit-unstage
   "lk" 'magit-discard
   "li" 'magit-commit-add-log)

  (general-define-key
   :keymaps 'magit-diff-mode-map
   :states '(normal visual)
   "lj" 'magit-jump-to-diffstat-or-diff
   "ld" 'magit-ediff-dwim
   "lc" 'magit-diff-while-committing)

  (general-define-key
   :keymaps 'magit-file-mode-map
   :states '(normal visual)
   "lg" 'magit-file-dispatch
   "lG" 'magit-dispatch
   "SPC dg" 'magit-diff
   "SPC dG" 'magit-ediff
   "N" 'magit-blob-next
   "H" 'magit-blob-previous)

(general-define-key
   :keymaps 'magit-mode-map
   :states '(normal visual)
   [return] 'magit-visit-thing
   [C-return] 'magit-jump-to-diffstat-or-diff
   "<tab>" 'magit-section-toggle
   [C-tab] 'magit-section-cycle
   [M-tab] 'magit-section-cycle-diffs
   [S-tab] 'magit-section-cycle-global
   "C" 'magit-section-backward
   "T" 'magit-section-forward
   "H" 'magit-go-backward
   "N" 'magit-go-forward
   "d" 'magit-delete-thing
   "z" 'magit-revert-no-commit
   "Q" 'magit-log-bury-buffer
   "SPC dg" 'magit-diff
   "SPC dG" 'magit-ediff
   "<f5>" 'magit-refresh)

  (general-define-key
   :keymaps 'magit-status-mode-map
   :states 'normal
   "p" 'magit-push
   "r" 'magit-rebase
   "f" 'magit-fetch
   "F" 'magit-pull
   "a" 'magit-log

   "l"  '(:ignore t :which-key "go")
   "l$" 'magit-process-buffer
   "lA" 'magit-cherry-pick
   "lb" 'magit-branch
   "lB" 'magit-bisect
   "lc" 'magit-commit
   "lC" 'magit-clone
   "ld" 'magit-diff
   "lD" 'magit-diff-refresh
   "le" 'magit-ediff-dwim
   "lE" 'magit-ediff
   "lf" 'magit-fetch
   "lF" 'magit-pull
   "lg" 'magit-refresh
   "lG" 'magit-refresh-all
   "lh" 'magit-dispatch
   "lk" 'magit-delete-thing
   "l?" 'magit-dispatch
   "ll" 'magit-log
   "lL" 'magit-log-refresh
   "lm" 'magit-merge
   "lM" 'magit-remote
   "lo" 'magit-submodule
   "lO" 'magit-subtree
   "lq" 'magit-mode-bury-buffer
   "lp" 'magit-push
   "lr" 'magit-rebase
   "lR" 'magit-file-rename
   "lt" 'magit-tag
   "lT" 'magit-notes
   "ls" 'magit-stage-file
   "lS" 'magit-stage-modified
   "lu" 'magit-unstage-file
   "lU" 'magit-unstage-all
   "lv" 'magit-revert-no-commit
   "lV" 'magit-revert
   "lw" 'magit-am
   "lW" 'magit-patch
   "lx" 'magit-reset-quickly
   "lX" 'magit-reset
   "ly" 'magit-show-refs
   "lY" 'magit-cherry
   "lz" 'magit-stash
   "lZ" 'magit-stash
   "l:" 'magit-git-command
   "l!" 'magit-run)

  ;; Make sure all environment variables are set
  (unless (fboundp 'exec-path-from-shell)
	(require 'exec-path-from-shell))

  ;; Active the blob mode automatically when we visit a file in a git project
  (add-hook 'magit-file-mode-hook 'magit-blob-mode)

  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (setq magit-status-show-hashes-in-headers t)

  (set-face-attribute 'magit-section-heading nil
					  :background nil
					  :foreground chocolate-theme-element)
  (set-face-attribute 'magit-section-highlight nil
					  :inherit 'region
					  :foreground nil
					  :background chocolate-theme-shadow)
  (set-face-attribute 'magit-diff-hunk-heading
					  nil :inherit 'lazy-highlight
					  :background chocolate-theme-shadow
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-diff-hunk-heading-highlight
					  nil :inherit 'lazy-highlight
					  :background chocolate-theme-shadow
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-diff-context
					  nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'magit-diff-context-highlight
					  nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'magit-blame-heading
					  nil :inherit 'secondary-selection
					  :background chocolate-theme-shadow+2
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-blame-highlight
					  nil :inherit 'lazy-highlight
					  :background chocolate-theme-shadow+2
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-head nil :foreground chocolate-theme-element+4)
  (set-face-attribute 'magit-branch-remote nil
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-branch-remote-head nil
					  :foreground chocolate-theme-highlight+2)
  ;; (set-face-attribute 'magit-diff-added-highlight nil
  ;; 					  :foreground chocolate-theme-bg
  ;; 					  :background chocolate-theme-element+3)
  ;; (set-face-attribute 'magit-diff-removed-highlight nil
  ;; 					  :background chocolate-theme-highlight)
  ;; (set-face-attribute 'magit-diffstat-removed nil
  ;;:foreground chocolate-theme-highlight)
  ;; (set-face-attribute 'magit-diffstat-removed nil
  ;; :foreground chocolate-theme-highlight)
  ;; (set-face-attribute 'magit-diffstat-added nil :foreground "#8ae234")
  )

