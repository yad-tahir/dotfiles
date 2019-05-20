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

;; Used in do--hs-toggle-hiding
(defvar do--hs-toggle-all)

(declare-function do--hs-toggle-hiding nil)

(eval-and-compile
  (unless (fboundp 'whitespace)
	(require 'whitespace)))

(setq fill-column 84
	  whitespace-line-column 84
	  whitespace-style (quote;; disable 'lines' style
						(face tabs spaces trailing
							  space-before-tab newline
							  indentation empty space-after-tab
							  space-mark newline-mark))
	  ;; whitespace-style (quote;; disable 'lines' style
	  ;; 					(face tabs spaces trailing
	  ;; 						  space-before-tab newline
	  ;; 						  indentation empty space-after-tab
	  ;; 						  tab-mark space-mark newline-mark))
	  )
(add-hook 'find-file-hook 'whitespace-mode)
(global-visual-line-mode 1) ;; wrap text lines

(defun do--hs-init ()
  "Add shortcuts for the hide-show mode"
  (eval-and-compile
	(unless (fboundp 'hideshow)
	  (require 'hideshow)))
  (hs-minor-mode 1)

  (defun do--hs-toggle-hiding ()
	(interactive)
	(if do--hs-toggle-all
		(prog1
			(hs-show-all)
		  (setq do--hs-toggle-all nil))
	  (prog1
		  (hs-hide-all)
		(setq do--hs-toggle-all t))))

  (general-define-key
   ;; :prefix "l"
   :keymaps 'hs-minor-mode-map
   :states '(normal visual)
   "<tab>" 'hs-toggle-hiding
   "M-<tab>" 'do--hs-toggle-hiding))
(add-hook 'prog-mode-hook  'do--hs-init)

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

(use-package highlight-parentheses
  :hook ((prog-mode . highlight-parentheses-mode))
  :ensure t
  :config
  (setq hl-paren-colors
		(list
		 chocolate-theme-highlight chocolate-theme-element
		 chocolate-theme-element+4 chocolate-theme-element+8
		 chocolate-theme-highlight+1 chocolate-theme-element+1
		 chocolate-theme-element+5 chocolate-theme-element+9
		 chocolate-theme-highlight+2 chocolate-theme-element+2
		 chocolate-theme-element+6 chocolate-theme-element+10
		 chocolate-theme-highlight+2 chocolate-theme-element+3
		 chocolate-theme-element+7 chocolate-theme-element+11)

		hl-paren-highlight-adjacent t))

;; (use-package highlight-thing
;; 	:hook ((prog-mode . highlight-thing-mode))
;; 	:config
;; 	(setq highlight-thing-delay-seconds 0
;; 		  highlight-thing-exclude-thing-under-point t)
;; 	(set-face-attribute 'highlight-thing nil
;; 						:background nil :foreground nil
;; 						:inherit nil :underline t))

;; (use-package evil-quickscope
;; 	:hook ((prog-mode . evil-quickscope-mode))
;; 	:config
;; 	(setq evil-quickscope-bidirectional t))

;; (use-package evil-visual-mark-mode
;; 	:hook ((prog-mode . evil-visual-mark-mode))
;; 	:config
;; 	(set-face-attribute 'evil-visual-mark-face nil :background nil :foreground
;; 						nil :inherit 'isearch :underline nil))
