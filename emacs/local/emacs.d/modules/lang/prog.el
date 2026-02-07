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


(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :config
  (general-define-key
   :keymaps 'flymake-mode-map
   :states  'normal
   "lf" '(flymake-show-buffer-diagnostics :which-key "flymake-check"))
  (general-define-key
   :keymaps 'flymake-diagnostics-buffer-mode-map
   :states 'normal
   "RET" 'flymake-goto-diagnostic))

(use-package eglot ;; Prepare language client for other modes
  :ensure t
  :commands (eglot)
  :config
  (general-define-key
   :keymaps 'eglot-mode-map
   :states 'normal
   "la" '(eglot-code-actions :which-key "code actions")
   "lr" '(eglot-rename :which-key "rename symbol")
   "lb" '(eglot-format :which-key "format buffer")
   "lH" '(eldoc :which-key "hover help")

   ;; Navigation
   "gd" '(xref-find-definitions :which-key "definition")
   "gr" '(xref-find-references :which-key "references")
   "gi" '(eglot-find-implementation :which-key "implementation"))

  (setq eglot-events-buffer-config '(:size 0 :format full)
        eglot-connect-timeout 120
        mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] "))
                mode-line-misc-info)))

(use-package devdocs ;; Better documentations
  :ensure t
  :commands (devdocs-lookup)
  :init
  (general-define-key
   :keymaps 'eglot-mode-map
   :states 'normal
   "lh" 'devdocs-lookup
   "<f1> f" 'devdocs-lookup
   "<f1> v" 'devdocs-lookup))

;; (use-package highlight-indent-guides
;;   :hook ((prog-mode . highlight-indent-guides-mode))
;;   :config
;;   (setq highlight-indent-guides-method 'character))

(provide 'do-prog)
