;;; -*- lexical-binding: t; -*-

(defun do--elisp-init()

  (cl-eval-when (compile)
	(require 'company))
  (with-eval-after-load 'company
	(set (make-local-variable 'company-backends)
		 (add-to-list 'company-backends 'company-elisp t))))

(add-hook 'emacs-lisp-mode-hook #'do--elisp-init())

(use-package macrostep
  :ensure t
  :commands (macrostep-expand macrostep-collapse)
  :after (evil)
  :init
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states 'normal
   :prefix "l"
   "m" '(:ignore t :which-key "micro")
   "me" 'macrostep-expand
   "mc" 'macrostep-collapse)

  :config
  (set-face-attribute 'macrostep-expansion-highlight-face nil
					  :background "#2f0a0a")
  )

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
