;;; -*- lexical-binding: t; -*-

(use-package web-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[p]html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))

(use-package emmet-mode
  :ensure t
  :config
  (general-define-key
   :keymaps 'emmet-mode-keymap
   :states '(normal insert)
   "M-<tab>" #'emmet-expand-yas
   "<tab>" #'do--tab-indent-or-complete)
  (add-hook 'emmet-mode-hook #'(lambda ()(yas-minor-mode 1)))
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'sgml-mode-hook #'emmet-mode))

(with-eval-after-load 'web-mode
  (use-package impatient-mode
	:ensure t
	:after (web-mode)
	:config
	(httpd-start)
	(add-hook 'html-mode-hook #'impatient-mode)
	(add-hook 'web-mode-hook #'impatient-mode)))
