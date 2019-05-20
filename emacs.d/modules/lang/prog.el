;;; -*- lexical-binding: t; -*-

;;; Code:
(defun do--prog-mode-init()
  "Called when the programming mode is activated."
  ;; Adjust word to be more symatic
  ;; (with-eval-after-load 'evil
  ;;   (defalias #'forward-evil-word #'forward-evil-symbol))

  (cl-eval-when (compile)
	(require 'company))
  (with-eval-after-load 'company
	(set (make-local-variable 'company-backends)
		 (add-to-list 'company-backends
					  '(company-keywords
						company-dabbrev-code
						company-dabbrev) t)))

  (eldoc-mode 1)
  ;; Make long-tailing lines visible
  (set (make-local-variable 'whitespace-style)
     (add-to-list 'whitespace-style 'lines)))
(add-hook 'prog-mode-hook  #'do--prog-mode-init)

;; (defun do--flycheck-init()
;;   "This function has been created to reduce the startup time caused by
;; flycheck, a badly optimized package!"

(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode))
  :config
  (set-face-attribute 'flycheck-warning nil
            :underline (list :color chocolate-theme-shadow+3 :style 'wave)))

  ;; display tips in popups
  ;;   (use-package flycheck-pos-tip
  ;;   	:hook ((flycheck-mode . flycheck-pos-tip-mode)))


;; (add-hook 'prog-mode-hook #'do--flycheck-init)

;; (use-package highlight-indent-guides
;;   :hook ((prog-mode . highlight-indent-guides-mode))
;;   :config
;;   (setq highlight-indent-guides-method 'character))
