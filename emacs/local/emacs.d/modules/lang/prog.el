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

;;; Code:
(defun do--prog-mode-init()
  "Called when the programming mode is activated."
  ;; Adjust word to be more symatic
  ;; (with-eval-after-load 'evil
  ;;   (defalias #'forward-evil-word #'forward-evil-symbol))

  (eldoc-mode 1)
  (with-eval-after-load 'whitespace
    ;; Make long-tailing lines visible
    (set (make-local-variable 'whitespace-style)
         (add-to-list 'whitespace-style 'lines-tail))))
(add-hook 'prog-mode-hook  #'do--prog-mode-init)

(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode))
  :config
  (general-define-key
   :map 'prog-mode-map
   :states '(normal visual)
   "lf" 'flycheck-list-errors))

;; (use-package highlight-indent-guides
;;   :hook ((prog-mode . highlight-indent-guides-mode))
;;   :config
;;   (setq highlight-indent-guides-method 'character))

(use-package eglot
  :ensure t
  :commands (eglot eglot-mode)
  :config
  (add-hook 'eglot-connect-hook #'turn-on-eldoc-mode))

(use-package eldoc-box
  :ensure t
  :after (evil eglot)
  :commands (eldoc-box-eglot-help-at-point)
  :init
  (setq evil-lookup-func #'eldoc-box-eglot-help-at-point)
  :config
  (set-face-attribute 'eldoc-box-border nil
                      :background chocolate-theme-highlight))

(provide 'do-prog)
