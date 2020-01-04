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
  :defer 10
  :ensure t
  :functions evil-set-initial-state
  :commands (magit-status magit-file-dispatch magit-dispatch)

  :init
  (general-define-key
   :keymaps 'override
   :states 'normal
   "SPC ag" 'magit-status
   "SPC aG" '(lambda () (interactive)
			   ;; Set 'universal argument' to force magit to prompt the user to
			   ;; choose a new repo location
			   (let ((current-prefix-arg "(4)"))
				 (call-interactively 'magit-status))))

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
   [remap evil-save-and-quit] 'magit-log-select-pick
   [remap evil-quit] 'magit-log-select-quit)

  (general-define-key
   :keymaps 'magit-log-mode-map
   :states 'normal
   "SPC l@" 'magit-checkout
   "SPC lr" 'magit-rebase
   "SPC lv" 'magit-revert
   "SPC lm" 'magit-merge
   "SPC lx" 'magit-reset
   "SPC ly" 'magit-cherry-pick
   "SPC lp" 'magit-log-move-to-parent
   "SPC ls" 'magit-log-toggle-commit-limit
   "SPC l+" 'magit-log-double-commit-limit
   "SPC l-" 'magit-log-half-commit-limit)

  (general-define-key
   :keymaps 'git-rebase-mode-map
   :states 'normal
   [remap evil-quit] 'kill-buffer
   [remap evil-save-and-quit] 'server-edit
   "<RET>"    'git-rebase-show-commit
   "SPC lp"   'git-rebase-pick
   "SPC l,"   'git-rebase-kill-line
   "SPC lb"   'git-rebase-break
   "SPC le"   'git-rebase-edit
   "SPC ll"   'git-rebase-label
   "SPC lm"   'git-rebase-merge
   "SPC lM"   'git-rebase-merge-toggle-editmsg
   "SPC lf"   'git-rebase-fixup
   "SPC lk"   'git-rebase-reword
   "SPC ls"   'git-rebase-squash
   "SPC lt"   'git-rebase-reset
   "SPC lx"   'git-rebase-exec
   "SPC li"   'git-rebase-insert
   "SPC lz"   'git-rebase-noop
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
   "<RET>"       'magit-visit-thing
   "SPC l <RET>" 'magit-jump-to-diffstat-or-diff
   "a"           'magit-apply
   "A"           'magit-commit-add-log
   "s"           'magit-stage
   "u"           'magit-unstage
   "k"           'magit-discard
   "d"           'magit-delete-thing
   "SPC la"      'magit-apply
   "SPC lA"      'magit-commit-add-log
   "SPC ls"      'magit-stage
   "SPC lu"      'magit-unstage
   "SPC lk"      'magit-discard)

  (general-define-key
   :keymaps 'magit-diff-mode-map
   :states '(normal visual)
   "SPC lj" 'magit-jump-to-diffstat-or-diff
   "SPC ld" 'magit-ediff-dwim
   "SPC lc" 'magit-diff-while-committing)

  (general-define-key
   :keymaps 'magit-file-mode-map
   :states '(normal visual)
   "SPC lg" 'magit-file-dispatch
   "SPC lG" 'magit-dispatch
   "SPC dg" 'magit-diff
   "SPC dG" 'magit-ediff
   "C-n"    'magit-blob-next
   "C-h"    'magit-blob-previous)

  (general-define-key
   :keymaps 'magit-mode-map
   :states '(normal visual)
   "<RET>"       'magit-visit-thing
   "SPC l <RET>" 'magit-jump-to-diffstat-or-diff
   "TAB"         'magit-section-toggle
   "<C-tab>"     'magit-section-cycle
   "C-M-i"       'magit-section-cycle-diffs
   "S-M-i"       'magit-section-cycle-global
   "C"           'magit-section-backward
   "T"           'magit-section-forward
   "H"           'magit-go-backward
   "N"           'magit-go-forward
   "d"           'magit-delete-thing
   "SPC dg"      'magit-diff
   "SPC dG"      'magit-ediff
   "<f5>"        'magit-refresh)

  (general-define-key
   :keymaps 'magit-status-mode-map
   :states 'normal
   [remap evil-quit] 'magit-mode-bury-buffer
   "SPC l$" 'magit-process-buffer
   "SPC l@" 'magit-checkout
   "SPC lb" 'magit-branch
   "SPC lB" 'magit-bisect
   "SPC lc" 'magit-commit
   "SPC lC" 'magit-clone
   "SPC ld" 'magit-diff
   "SPC lD" 'magit-diff-refresh
   "SPC le" 'magit-ediff-dwim
   "SPC lE" 'magit-ediff
   "SPC lf" 'magit-fetch
   "SPC lF" 'magit-pull
   "SPC lg" 'magit-refresh
   "SPC lG" 'magit-refresh-all
   "SPC l?" 'magit-dispatch
   "SPC lh" 'magit-dispatch
   "SPC lk" 'magit-delete-thing
   "SPC ll" 'magit-log
   "SPC lL" 'magit-log-refresh
   "SPC lm" 'magit-merge
   "SPC ln" 'magit-show-refs
   "SPC lN" 'magit-remote
   "SPC lo" 'magit-submodule
   "SPC lO" 'magit-subtree
   "SPC lp" 'magit-push
   "SPC lr" 'magit-rebase
   "SPC lR" 'magit-file-rename
   "SPC lt" 'magit-tag
   "SPC lT" 'magit-notes
   "SPC ls" 'magit-stage-file
   "SPC lS" 'magit-stage-modified
   "SPC lu" 'magit-unstage-file
   "SPC lU" 'magit-unstage-all
   "SPC lv" 'magit-revert
   "SPC lw" 'magit-am
   "SPC lW" 'magit-patch
   "SPC lx" 'magit-reset
   "SPC ly" 'magit-cherry-pick
   "SPC lY" 'magit-cherry
   "SPC lz" 'magit-stash
   "SPC l:" 'magit-git-command
   "SPC l!" 'magit-run)

  ;; Make sure all environment variables are set
  (unless (fboundp 'exec-path-from-shell)
	(require 'exec-path-from-shell))

  ;; Activate the blob mode automatically when we visit a file in a git project
  (add-hook 'magit-file-mode-hook 'magit-blob-mode)

  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (setq magit-status-show-hashes-in-headers t
		magit-blame-echo-style 'margin)

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
