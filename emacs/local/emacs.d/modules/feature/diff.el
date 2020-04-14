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

(use-package diff-hl
  :ensure t
  :hook ((magit-file-mode . diff-hl-mode)
		 (dired-mode . diff-hl-dired-mode))
  :config
  (general-define-key
   :kemaps 'diff-hl-mode-map
   :states '(normal visual)
   "C-t" 'diff-hl-next-hunk
   "C-c" 'diff-hl-previous-hunk)

  (set-face-attribute 'diff-hl-change nil
					  :foreground 'nil
					  :inherit 'diff-changed
					  :background chocolate-theme-highlight+2)

  (set-face-attribute 'diff-hl-insert nil
					  :foreground 'nil
					  :inherit 'diff-added
					  :background chocolate-theme-element)

  (set-face-attribute 'diff-hl-delete nil
					  :foreground 'nil
					  :inherit 'diff-removed
					  :background chocolate-theme-highlight)

  (with-eval-after-load 'magit
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package smerge-mode
  :defer t
  :config
  (general-define-key
   :keymaps 'smerge-mode-map
   "l<" 'smerge-keep-upper
   "l>" 'smerge-keep-lower
   "l SPC" 'smerge-keep-current
   "la" 'smerge-keep-all
   "lr" 'smerge-refine
   "ld" 'smerge-ediff
   "lb" 'smerge-keep-base))

(use-package ediff
  :defer t
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC d" '(:ignore t :which-key "diff")
   "SPC db" 'ediff-buffers
   "SPC dB" 'ediff-buffers3
   "SPC dd" 'ediff-directories
   "SPC dD" 'ediff-directories3
   "SPC df" 'ediff-files
   "SPC dF" 'ediff-files3
   "SPC dc" 'ediff-current-file
   "SPC dp" 'epatch-buffer
   "SPC dP" 'epatch
   "SPC dm" 'ediff-merge-buffers
   "SPC dM" 'ediff-merge-files)
  :config
  (evil-set-initial-state 'ediff-mode 'emacs)
  (setq ediff-split-window-function 'split-window-horizontally)
  ;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-highlight-all-diffs t)
  (setq ediff-use-long-help-message t)
  (setq ediff-auto-refine t))


(provide 'do-diff)
