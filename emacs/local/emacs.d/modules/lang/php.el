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

(use-package php-mode
  :ensure t
  :mode "\\.php[s345]?$"
  :mode "\\.inc$"
  :interpreter "php")

;; (with-eval-after-load 'php-mode
;;   (use-package company-php
;;	:ensure t)
;;   (defun do--php-init()
;;	;; (flymake-php-load)
;;	(with-eval-after-load 'company
;;	  (ac-php-core-eldoc-setup)

;;	  (ac-php-mode 1)
;;	  (set (make-local-variable 'company-backends)
;;		   (append company-backends '((
;;									   company-ac-php-backend
;;									   :with company-dabbrev-code
;;									   :with company-dabbrev
;;									   :with company-keywords
;;									   ))))))
;;   (add-hook 'php-mode-hook #'do--php-init()))


(provide 'do-php)
