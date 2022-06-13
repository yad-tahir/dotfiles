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

(use-package magit
  :ensure t
  :functions evil-set-initial-state
  :commands (magit-status magit-file-dispatch magit-dispatch)
  :init
  (general-define-key
   :keymaps 'override
   :states 'normal
   "SPC g" '(magit-status :which-key "magit")
   "SPC G" '((lambda () (interactive)
			  ;; Set 'universal argument' to force magit to prompt the user to
			  ;; choose a new repo location
			  (let ((current-prefix-arg "(4)"))
				(call-interactively 'magit-status)))
			  :which-key "magit+"))

  :config
  ;; Remove conflicting key bindings
  (general-define-key
   :keymaps '(magit-file-section-map
			  magit-hunk-section-map
			  magit-log-mode-map)
   "C"   'nil
   "C-c" 'nil)
  ;; Remove all keybindings in the blob mode to avoid future bugs
  (setf (cdr magit-blob-mode-map) nil)

  (general-define-key
   :keymaps 'magit-log-select-mode-map
   :states '(normal visual)
   "SPC lw" 'magit-log-select-pick
   "SPC lq" 'magit-log-select-quit)

  (general-define-key
   :keymaps 'magit-log-mode-map
   :states 'normal
   "l@" 'magit-checkout
   "lr" 'magit-rebase
   "lv" 'magit-revert
   "lm" 'magit-merge
   "lx" 'magit-reset
   "ly" 'magit-cherry-pick
   "lp" 'magit-log-move-to-parent
   "ls" 'magit-log-toggle-commit-limit
   "l+" 'magit-log-double-commit-limit
   "l-" 'magit-log-half-commit-limit)

  (general-define-key
   :keymaps 'git-rebase-mode-map
   :states 'normal
   "SPC lq" 'kill-buffer
   "SPC lw" 'server-edit
   "<RET>" 'git-rebase-show-commit
   "lp"    'git-rebase-pick
   "l,"    'git-rebase-kill-line
   "lb"    'git-rebase-break
   "le"    'git-rebase-edit
   "ll"    'git-rebase-label
   "lm"    'git-rebase-merge
   "lM"    'git-rebase-merge-toggle-editmsg
   "lf"    'git-rebase-fixup
   "lk"    'git-rebase-reword
   "ls"    'git-rebase-squash
   "lt"    'git-rebase-reset
   "lx"    'git-rebase-exec
   "li"    'git-rebase-insert
   "lz"    'git-rebase-noop
   "M-c"   'git-rebase-move-line-up
   "M-t"   'git-rebase-move-line-down
   "z"     'git-rebase-undo)

  (general-define-key
   :keymaps '(magit-file-section-map
			  magit-untracked-section-map
			  magit-unstaged-section-map
			  magit-unmerged-section-map
			  magit-merge-preview-mode-map
			  magit-staged-section-map
			  magit-hunk-section-map)
   "<RET>"   'magit-visit-thing
   "l <RET>" 'magit-jump-to-diffstat-or-diff
   "a"       'magit-apply
   "A"       'magit-commit-add-log
   "s"       'magit-stage
   "u"       'magit-unstage
   "k"       'magit-discard
   "d"       'magit-delete-thing
   "la"      'magit-apply
   "lA"      'magit-commit-add-log
   "ls"      'magit-stage
   "lu"      'magit-unstage
   "lk"      'magit-discard)

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
   "C-n"    'magit-blob-next
   "C-h"    'magit-blob-previous)

  (general-define-key
   :keymaps 'magit-mode-map
   :states '(normal visual)
   "<RET>"   'magit-visit-thing
   "l <RET>" 'magit-jump-to-diffstat-or-diff
   "TAB"     'magit-section-toggle
   "<C-tab>" 'magit-section-cycle
   "C-M-i"   'magit-section-cycle-diffs
   "S-M-i"   'magit-section-cycle-global
   "C"       'magit-section-backward
   "T"       'magit-section-forward
   "H"       'magit-go-backward
   "N"       'magit-go-forward
   "d"       'magit-delete-thing
   "SPC dg"  'magit-diff
   "SPC dG"  'magit-ediff
   "<f5>"    'magit-refresh)

  (general-define-key
   :keymaps 'magit-status-mode-map
   :states 'normal
   "SPC lw" 'magit-mode-bury-buffer
   "l$" 'magit-process-buffer
   "l@" 'magit-checkout
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
   "l?" 'magit-dispatch
   "lh" 'magit-dispatch
   "lk" 'magit-delete-thing
   "ll" 'magit-log
   "lL" 'magit-log-refresh
   "lm" 'magit-merge
   "ln" 'magit-show-refs
   "lN" 'magit-remote
   "lo" 'magit-submodule
   "lO" 'magit-subtree
   "lp" 'magit-push
   "lr" 'magit-rebase
   "lR" 'magit-file-rename
   "lt" 'magit-tag
   "lT" 'magit-notes
   "ls" 'magit-stage-file
   "lS" 'magit-stage-modified
   "lu" 'magit-unstage-file
   "lU" 'magit-unstage-all
   "lv" 'magit-revert
   "lw" 'magit-am
   "lW" 'magit-patch
   "lx" 'magit-reset
   "ly" 'magit-cherry-pick
   "lY" 'magit-cherry
   "lz" 'magit-stash
   "l:" 'magit-git-command
   "l!" 'magit-run)

  ;; Make sure all environment variables are set
  (unless (fboundp 'exec-path-from-shell)
	(require 'exec-path-from-shell))

  ;; Activate the blob mode automatically when we visit a file in a git project
  (add-hook 'magit-file-mode-hook 'magit-blob-mode)

  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (setq magit-status-show-hashes-in-headers t
		magit-blame-echo-style 'margin)

  (add-hook 'magit-status-mode-hook 'do-line-numbers-to-visual)
  (add-hook 'magit-log-mode-hook 'do-line-numbers-to-visual)

  (set-face-attribute 'magit-section-heading nil
					  :background nil
					  :foreground chocolate-theme-element)
  (set-face-attribute 'magit-section-highlight nil
					  :inherit 'region
					  :foreground nil
					  :background chocolate-theme-shadow+1)
  (set-face-attribute 'magit-diff-hunk-heading
					  nil :inherit 'lazy-highlight
					  :background chocolate-theme-shadow+1
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-diff-hunk-heading-highlight
					  nil :inherit 'lazy-highlight
					  :background chocolate-theme-shadow+1
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
  (set-face-attribute 'magit-dimmed nil
					  :inherit 'shadow
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'magit-hash nil
					  :inherit 'shadow
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'magit-log-date nil
					  :foreground chocolate-theme-white+3)
  (set-face-attribute 'magit-log-graph nil
					  :foreground chocolate-theme-white+3))

(provide 'do-git)
