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

(defvar-local do--company-whitespace-state nil
  "Stores whether whitespace-mode was active before company-mode started.")

(use-package company
  :ensure t
  :hook ((find-file . company-mode))
  :commands (company-complete-common company-complete)
  :init
  (general-define-key
   :states 'insert
   ;; C-M-i is used instead of M-TAB to support terminal Emacs
   "C-M-i" 'company-complete)

  :config
  ;; Remove compiler warnings
  (require 'company-dabbrev)
  (require 'company-dabbrev-code)

  ;; Key bindings
  (general-define-key
   :keymaps 'company-active-map
   "M-n" 'nil
   "M-p" 'nil
   "<RET>" 'company-complete-selection
   ;; In Emacs Terminal
   "TAB" 'company-complete-common-or-cycle
   "<escape>" 'company-abort
   "C-f" 'company-filter-candidates
   "C-e" 'counsel-company
   "C-t" 'company-select-next
   "C-c" 'company-select-previous)

  (general-define-key
   :keymaps 'company-search-map
   "M-n" 'nil
   "M-p" 'nil
   "<escape>" 'company-abort
   "C-e" 'counsel-company
   "C-t" 'company-select-next
   "C-c" 'company-select-previous)

  (setq company-idle-delay nil
        company-minimum-prefix-length 1
        company-text-icons-add-background t
        company-search-filtering t
        company-tooltip-limit 15
        company-tooltip-margin 2 ;; increasing it above 2 will bug out org-roam suggestions
        company-tooltip-width-grow-only t
        company-tooltip-maximum-width 60
        company-tooltip-minimum-width (/ company-tooltip-maximum-width 2)
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-tooltip-flip-when-above t
        company-global-modes '(not eshell-mode shell-mode term-mode erc-mode
                                   message-mode help-mode gud-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  ;; Default backends
  ;; the order of the groups is important. If results are found in the first group,
  ;; then company will not trigger the second group and so forth.
  (setq company-backends '(company-files
                           (company-capf company-ispell :separate)
                           (company-dabbrev-code
                            company-gtags
                            company-etags company-keywords
                            company-oddmuse company-dabbrev)
                           company-ispell))

  ;; BUG-FIX: company looks ugly with whitespace mode
  (add-hook 'company-completion-started-hook
            #'(lambda (&optional _result)
                (setq do--company-whitespace-state
                      (bound-and-true-p whitespace-mode))
                (when do--company-whitespace-state
                  (whitespace-mode -1))))
  (add-hook 'company-after-completion-hook
            #'(lambda (&optional _result)
                (when do--company-whitespace-state
                  (whitespace-mode 1)
                  (setq do--company-whitespace-state nil))))

  (defun do--evil-repeat-suppress-flyspell (orig-fn &rest args)
    "Temporarily disable flyspell-mode serious prevent lag.

The lag specially occurs on evil-repeat playing company-complete records."
    ;; We let-bind flyspell-mode to nil. The hook `flyspell-post-command-hook`
    ;; checks this variable, sees it is nil, and exits immediately.
    (let ((flyspell-mode nil))
      (apply orig-fn args)))
  (advice-add 'company-complete :around 'do--evil-repeat-suppress-flyspell)
  (advice-add 'evil-repeat :around 'do--evil-repeat-suppress-flyspell))

(use-package company-statistics
  :ensure t
  :disabled t
  :after (company)
  :config
  (setq company-statistics-file
        (concat "~/.emacs.d/.cache/" "company-stats-cache.el")))

(use-package company-quickhelp
  :ensure t
  :disabled t
  :after (company)
  :config
  (setq company-quickhelp-delay nil
        company-quickhelp-color-foreground chocolate-theme-white+2
        company-quickhelp-color-background chocolate-theme-shadow+1)

  (general-define-key
   :keymaps 'company-active-map
   "C-h" 'company-quickhelp-manual-begin))

;; Language server client as a company-backend
;; (use-package company-lsp
;;   :ensure t
;;   :commands (company-lsp)
;;   :config
;;   (setq company-lsp-enable-snippet nil))

(provide 'do-company)
