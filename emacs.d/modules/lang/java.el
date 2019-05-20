;;; -*- lexical-binding: t; -*-


(use-package eclim
  :hook ((java-mode-hook . eclim-mode))
  :init
  (custom-set-variables '(eclim-eclipse-dirs '("/usr/lib/eclipse"))
						'(eclim-executable "/usr/lib/eclipse/eclim")
						'(eclimd-default-workspace "/home/yad/project/eclipse"))
  :config
  (setq eclim-autostart t))

  (use-package company-emacs-eclim
	:after (:all company eclim)
	:ensure t
	:config
	(company-emacs-eclim-setup))
