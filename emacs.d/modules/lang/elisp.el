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

(defun do--elisp-init()

  (cl-eval-when (compile)
	(require 'company))
  (with-eval-after-load 'company
	(set (make-local-variable 'company-backends)
		 (add-to-list 'company-backends
					  '(;; Highest priority
						company-semantic
						company-capf
						company-files
						;; Current mode
						company-elisp
						;; Lowest priority - keep the ordering
						company-keywords
						company-dabbrev-code
						company-dabbrev
						company-ispell)
					  nil))))

(add-hook 'emacs-lisp-mode-hook #'do--elisp-init())

(use-package macrostep
  :ensure t
  :commands (macrostep-expand macrostep-collapse)
  :init
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states 'normal
   "xme" 'macrostep-expand
   "xmc" 'macrostep-collapse)

  :config
  (set-face-attribute 'macrostep-expansion-highlight-face nil
					  :background "#2f0a0a")
  )

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
