;;; -*- lexical-binding: t; -*-

(use-package flyspell
  :ensure t
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))
