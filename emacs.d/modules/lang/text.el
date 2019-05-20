;;; -*- lexical-binding: t; -*-

;;; Code:
(defun do--text-mode-init()
  (cl-eval-when (compile)
	(require 'company))
  (with-eval-after-load 'company
	(set (make-local-variable 'company-backends)
		 (add-to-list 'company-backends
					  '(company-ispell  company-dabbrev) t))))

(add-hook 'text-mode-hook  #'do--text-mode-init)
