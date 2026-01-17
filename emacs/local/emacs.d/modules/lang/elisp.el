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

;; Custom functions/operators
;; (defun do--elisp-init()
;; "Called whenever the emacs-lisp mode is loaded."

;; Remove compiler warnings
;; (eval-when-compile
;;	(require 'company))

;; (with-eval-after-load 'company
;;	(set (make-local-variable 'company-backends)
;;		 (add-to-list 'company-backends
;;					  '(;; Highest priority
;;						company-semantic
;;						company-capf
;;						company-files
;;						;; Lowest priority - keep the ordering
;;						company-keywords
;;						company-dabbrev-code
;;						company-dabbrev
;;						company-ispell)
;;					  nil))))

;; (add-hook 'emacs-lisp-mode-hook #'do--elisp-init())

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(evil-define-operator do-elisp-eval (beginning end)
  "Ask for a motion, and then evaluate the selected region accordingly."
  :move-point nil
  :motion do-evil-a-section
  (eval-region beginning end t))

;; Keybindings
(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :states '(normal visual)
 "le" 'do-elisp-eval)

;; Third-party packages
(use-package macrostep
  :ensure t
  :commands (macrostep-expand macrostep-collapse)
  :init
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal visual)
   "lm" 'macrostep-expand
   "lM" 'macrostep-collapse)

  :config
  (set-face-attribute 'macrostep-expansion-highlight-face nil
                      :background chocolate-theme-shadow+1)
  (general-define-key
   :keymaps 'macrostep-keymap
   "SPC lw" 'macrostep-collapse-all
   "SPC lq" 'macrostep-collapse-all))

(use-package package-lint
  :ensure t
  :commands (package-lint-current-buffer)
  :init
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal visual)
   "ll" 'package-lint-current-buffer))

(provide 'do-elisp)
