;;; -*- lexical-binding: t; -*-

(use-package php-mode
  :ensure t
  :mode "\\.php[s345]?$"
  :mode "\\.inc$"
  :interpreter "php"
  :config
  (require 'php-ext))

(with-eval-after-load 'php-mode
  (use-package company-php
	:ensure t)
  (defun do--php-init()
	;; (flymake-php-load)
	(with-eval-after-load 'company
	  (ac-php-core-eldoc-setup)

	  (ac-php-mode 1)
	  (set (make-local-variable 'company-backends)
		   (append company-backends '((
									   company-ac-php-backend
									   :with company-dabbrev-code
									   :with company-dabbrev
									   :with company-keywords
									   ))))))
  (add-hook 'php-mode-hook #'do--php-init()))
