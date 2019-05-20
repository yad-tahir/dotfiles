;;; -*- lexical-binding: t; -*-

;;; Code:
(use-package latte
  :defer 5
  :load-path "~/.emacs.d/local-packages/latte"
  :hook (
		 ;; (find-file . latte-mode)
		 (after-change-major-mode . latte-mode)
		 )

  :commands (latte-files latte-search
							   latte-insert-keyword latte-insert-org-tag
							   latte-grep-topic latte-new-entry
							   latte-grep-all)
  :init
  (setq latte-directory "~/notes/notebook"
		latte-scan-idle-delay 30
		latte-rehighlight-after-scan nil
		latte-ignore-words '("attach"))
  (general-define-key
   :prefix "SPC n"
   :keymaps 'override
   :states '(normal visual)
   "" '(:ignore t :which-key "notes")
   "f" #'latte-files
   "s" #'latte-search
   "k" #'latte-insert-keyword
   "#" #'latte-insert-org-tag
   "g" #'latte-grep-topic
   "c" #'latte-new-entry)

  (general-define-key
   :prefix "SPC s"
   :keymaps 'override
   :states '(normal visual)
   "n" #'latte-search)

;;;###autoload
  (defun latte()
	(interactive)
	(do-make-frame "notebook")
	(latte-search))

  :config

  ;; (set-face-attribute 'latte-keyword-face nil
  ;; 					  :foreground chocolate-theme-element+8)

  (with-eval-after-load 'org
	(general-def org-mode-map
	  :states '(normal visual)
	  "ln" #'(:ignore t :which-key "notes")
	  "lnk" #'latte-insert-keyword
	  "ln#" #'latte-insert-org-tag
	  "l#" #'latte-insert-org-tag
	  "lng" #'latte-grep-topic
	  "lg" #'latte-grep-topic
	  "lnG" #'latte-grep-all)))

(provide 'notebook)
