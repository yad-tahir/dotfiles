;;; -*- lexical-binding: t; -*-

(use-package define-word
  :ensure t
  :commands (define-word define-word-at-point)
  :init
  (general-define-key
   :prefix "SPC l"
   :states 'normal
   "w" #'define-word-at-point
   "W" #'define-word))

