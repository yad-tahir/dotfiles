;;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :defer 20
  :ensure t
  :config
    (setq exec-path-from-shell-arguments (list "-l"))
	(setq exec-path-from-shell-shell-name "sh")
    (setq exec-path-from-shell-check-startup-files nil)

    (add-to-list 'exec-path-from-shell-variables "SHELL")
    (add-to-list 'exec-path-from-shell-variables "GOPATH")
    (add-to-list 'exec-path-from-shell-variables "GNUPGHOME")
    (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")

    (exec-path-from-shell-initialize))
