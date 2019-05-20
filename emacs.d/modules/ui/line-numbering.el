;;; -*- lexical-binding: t; -*-

(use-package display-line-numbers
  :demand t
  :config
  (setq display-line-numbers-current-absolute nil
		;; Narrowing does not make sense in visual type
		display-line-numbers-widen t
		display-line-numbers-width 3
		;; Visual really useful for the evil mode
		display-line-numbers-type 'visual)

  ;; Theme
  (set-face-attribute 'line-number nil
					  :foreground chocolate-theme-shadow+3
					  :weight 'normal)
  (set-face-attribute 'line-number-current-line nil
					  :foreground chocolate-theme-bg
					  :background chocolate-theme-shadow+3
					  :weight 'normal)
  (set-face-attribute 'fringe nil
					  :background chocolate-theme-bg)

  ;; Start line numbering
  (add-hook 'change-major-mode-after-body-hook #'display-line-numbers-mode))
