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

(use-package whitespace
  :hook ((find-file . whitespace-mode))
  :config
  (setq whitespace-line-column fill-column
		whitespace-style (quote
						  (face trailing indentation empty
								newline newline-mark
								spaces space-mark
								space-before-tab space-after-tab))))

(use-package simple
  :preface
  (declare-function global-visual-line-mode nil)
  :demand t
  :config
  (global-visual-line-mode 1))

(use-package evil-commands
  :hook ((prog-mode . hs-minor-mode))
  :config
  (defvar do--fold-global-toggle t)

  (defun do--fold-toggle-all ()
	(interactive)
	(setq do--fold-global-toggle (not do--fold-global-toggle))
	(if do--fold-global-toggle
		(call-interactively 'evil-open-folds)
	  (call-interactively 'evil-close-folds)))

  (general-define-key
   :keymaps 'hs-minor-mode-map
   :states 'normal
   "TAB" 'evil-toggle-fold
   "C-M-i" 'do--fold-toggle-all))

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

(use-package highlight-parentheses
  :hook ((prog-mode . highlight-parentheses-mode))
  :ensure t
  :config
  (setq hl-paren-colors
		(list chocolate-theme-highlight chocolate-theme-element
			  chocolate-theme-element+4 chocolate-theme-element+8
			  chocolate-theme-highlight+1 chocolate-theme-element+1
			  chocolate-theme-element+5 chocolate-theme-element+9
			  chocolate-theme-highlight+2 chocolate-theme-element+2
			  chocolate-theme-element+6 chocolate-theme-element+10
			  chocolate-theme-highlight+2 chocolate-theme-element+3
			  chocolate-theme-element+7 chocolate-theme-element+11)
		hl-paren-highlight-adjacent t))

(use-package evil-quickscope
  :ensure t
  :disabled t
  :commands  (evil-quickscope-find-char
			  evil-quickscope-find-char-backward
			  evil-quickscope-find-char-to
			  evil-quickscope-find-char-to-backward)
  :init
  (general-define-key
   :states 'motion
   "f" 'evil-quickscope-find-char
   "F" 'evil-quickscope-find-char-backward
   "j" 'evil-quickscope-find-char-to
   "J" 'evil-quickscope-find-char-to-backward)

  :config
  (setq evil-quickscope-bidirectional nil
		evil-quickscope-cross-lines t
		evil-quickscope-disable-in-comments 'nil)

  (general-define-key
   :keymaps 'evil-quickscope-mode-map
   :states 'motion
   "t" 'nil
   "T" 'nil)

  (global-evil-quickscope-mode 1))


(provide 'do-text-visual)
