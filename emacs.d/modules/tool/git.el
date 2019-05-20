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
  :after (evil)
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
   :keymaps 'magit-mode-map
   :states '(normal visual)
   [return] 'magit-visit-thing
   [C-return] 'magit-dired-jump
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
   "<f5>" 'magit-refresh)

  (general-define-key
   :keymaps 'magit-log-mode-map
   :states '(normal visual)
   "p" 'magit-log-move-to-parent
   "s" 'magit-log-toggle-commit-limit
   "+" 'magit-log-double-commit-limit
   "-" 'magit-log-half-commit-limit
   "l" 'nil
   "lp" 'magit-log-move-to-parent
   "ls" 'magit-log-toggle-commit-limit
   "l+" 'magit-log-double-commit-limit
   "l-" 'magit-log-half-commit-limit)


  (general-define-key
   :keymaps '(magit-file-section-map
			  magit-unstaged-section-map
			  magit-staged-section-map
			  magit-hunk-section-map)
   "a" 'magit-apply
   "s" 'magit-stage
   "u" 'magit-unstage
   "d" 'magit-discard
   "i" 'magit-commit-add-log
   "d" 'magit-diff-trace-definition
   "e" 'magit-diff-edit-hunk-commit)


  (general-define-key
   :keymaps 'magit-diff-mode-map
   :states '(normal visual)
   "lj" 'magit-jump-to-diffstat-or-diff
   "lc" 'magit-diff-while-committing)

  (general-define-key
   :keymaps 'magit-file-mode-map
   :states '(normal visual)
   "lg" 'magit-file-dispatch
   "lG" 'magit-dispatch
   "N" 'magit-blob-next
   "H" 'magit-blob-previous)

  (general-define-key
   :keymaps 'magit-status-mode-map
   :prefix "l"
   :states '(normal visual)
   "$" 'magit-process-buffer
   "A" 'magit-cherry-pick
   "b" 'magit-branch
   "B" 'magit-bisect
   "c" 'magit-commit
   "C" 'magit-clone
   "d" 'magit-diff
   "D" 'magit-diff-refresh
   "e" 'magit-ediff-dwim
   "E" 'magit-ediff
   "f" 'magit-fetch
   "F" 'magit-pull
   "g" 'magit-refresh
   "G" 'magit-refresh-all
   "h" 'magit-dispatch
   "k" 'magit-delete-thing
   "?" 'magit-dispatch
   "l" 'magit-log
   "L" 'magit-log-refresh
   "m" 'magit-merge
   "M" 'magit-remote
   "o" 'magit-submodule
   "O" 'magit-subtree
   "q" 'magit-mode-bury-buffer
   "p" 'magit-push
   "r" 'magit-rebase
   "R" 'magit-file-rename
   "t" 'magit-tag
   "T" 'magit-notes
   "s" 'magit-stage-file
   "S" 'magit-stage-modified
   "u" 'magit-unstage-file
   "U" 'magit-unstage-all
   "v" 'magit-revert-no-commit
   "V" 'magit-revert
   "w" 'magit-am
   "W" 'magit-patch
   "x" 'magit-reset-quickly
   "X" 'magit-reset
   "y" 'magit-show-refs
   "Y" 'magit-cherry
   "z" 'magit-stash
   "Z" 'magit-stash
   ":" 'magit-git-command
   "!" 'magit-run)

  ;; Make sure all environment variables are set
  (unless (fboundp 'exec-path-from-shell)
	(require 'exec-path-from-shell))

  ;; Active the blob mode automatically when we visit a file in a git project
  (add-hook 'magit-file-mode-hook 'magit-blob-mode)

  (evil-set-initial-state 'magit-popup-mode 'emacs)

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

(use-package diff-hl
  :ensure t
  :defines (vc-git-diff-switches)
  :hook ((find-file . diff-hl-mode)
		 (dired-mode . diff-hl-dir-mode))
  :config
  (setq vc-git-diff-switches '("--histogram"))

  (general-define-key
   :kemaps 'diff-hl-mode-map
   :states '(normal visual)
   "T" 'diff-hl-next-hunk
   "C" 'diff-hl-previous-hunk)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
