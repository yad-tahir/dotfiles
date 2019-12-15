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

;; To silent warnings
(cl-eval-when (compile)
  (require 'company-dabbrev)
  (require 'company-dabbrev-code))

(use-package company
  :ensure t
  :hook ((find-file . company-mode))
  :commands (company-complete-common)
  :init
  (defun do--tab-complete ()
	(interactive)
	(if (or (save-excursion (looking-at "\\_>")) ;; end of symbol
			(save-excursion (looking-at "\\."))  ;; ends with dot
			(save-excursion (looking-at "->")))  ;; ends with ->
		(company-complete-common)
	  ;; Otherwise, the company completion list is most likely useless
	  (tab-to-tab-stop)))

  (general-define-key
   :states 'insert
   ;; make TAB smarter a bit!
   "<tab>" 'do--tab-complete
   "M-<tab>" 'tab-to-tab-stop)

  :config
  ;; Key bindings
  (general-define-key
   :keymaps 'company-active-map
   "M-n" 'nil
   "M-p" 'nil
   "<return>" 'company-complete-selection
   "<tab>" 'company-complete-common-or-cycle
   "<escape>" 'company-abort
   "C-f" 'company-filter-candidates
   "C-t" 'company-select-next
   "C-c" 'company-select-previous)

  (general-define-key
   :keymaps 'company-search-map
   "M-n" 'nil
   "M-p" 'nil
   "<escape>" 'company-abort
   "C-s" 'counsel-company
   "C-t" 'company-select-next
   "C-c" 'company-select-previous)

  (setq company-idle-delay nil
		company-minimum-prefix-length 1
		company-prefix nil
		company-auto-complete nil ;; To avoid selecting an item using SPC
		company-search-filtering t
		company-tooltip-limit 15
		company-dabbrev-downcase nil
		company-dabbrev-ignore-case t
		company-dabbrev-code-other-buffers t
		company-tooltip-align-annotations t
		company-require-match 'never
		company-global-modes '(not eshell-mode shell-mode term-mode erc-mode
								   message-mode help-mode gud-mode)
		company-frontends '(;; company-tng-frontend
							company-preview-if-just-one-frontend
							company-pseudo-tooltip-frontend
							company-echo-metadata-frontend))

  ;; Default backends
  (setq company-backends
		'((company-capf company-files company-dabbrev company-ispell)
		  (company-dabbrev-code company-gtags company-etags company-keywords)
		  company-oddmuse
		  company-dabbrev))

  (set-face-attribute 'company-scrollbar-bg nil
					  :background chocolate-theme-shadow+1)
  (set-face-attribute 'company-scrollbar-fg nil
					  :background chocolate-theme-shadow+3)
  (set-face-attribute 'company-preview nil
					  :inherit nil
					  :foreground chocolate-theme-highlight+1
					  :background chocolate-theme-bg)
  (set-face-attribute 'company-preview-common nil
					  :inherit nil
					  :foreground chocolate-theme-highlight+1)
  (set-face-attribute 'company-preview-search nil
					  :inherit nil
					  :background chocolate-theme-highlight+2
					  :foreground chocolate-theme-bg)
  (set-face-attribute 'company-tooltip nil
					  :foreground chocolate-theme-white
					  :background chocolate-theme-shadow+1)
  (set-face-attribute 'company-tooltip-annotation nil
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'company-tooltip-annotation-selection nil
					  :foreground chocolate-theme-bg)
  (set-face-attribute 'company-tooltip-common nil
					  :foreground chocolate-theme-highlight+2
					  :weight 'bold)
  (set-face-attribute 'company-tooltip-common-selection nil
					  :foreground chocolate-theme-bg
					  :weight 'bold
					  :background chocolate-theme-highlight+2)
  (set-face-attribute 'company-tooltip-search nil
					  :foreground chocolate-theme-highlight+2
					  :background nil
					  :underline nil
					  :weight 'bold)
  (set-face-attribute 'company-tooltip-search-selection nil
					  :foreground chocolate-theme-bg
					  :background chocolate-theme-highlight+2
					  :weight 'bold
					  :underline nil)
  (set-face-attribute 'company-tooltip-selection nil
					  :foreground chocolate-theme-bg
					  :background chocolate-theme-highlight+2))

(use-package company-flx
  :ensure t
  :after (company)
  :config
  (company-flx-mode 1))

(use-package company-statistics
  :ensure t
  :after (company)
  :config
  (setq company-statistics-file
		(concat "~/.emacs.d/.cache/" "company-stats-cache.el")))

(use-package company-quickhelp
  :ensure t
  :after (company)
  :config
  (setq company-quickhelp-delay nil
		company-quickhelp-color-foreground chocolate-theme-white+2
		company-quickhelp-color-background chocolate-theme-shadow+3)

  (general-define-key
   :keymaps 'company-active-map
   "C-h" 'company-quickhelp-manual-begin))

;; Language server client as a company-backend
(use-package company-lsp
  :ensure t
  :commands (company-lsp)
  :config
  (setq company-lsp-enable-snippet nil))

(provide 'do-company)
