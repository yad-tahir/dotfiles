;;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :ensure t
  :after (company)
  :commands (yas-global-mode yas-expand do--tab-indent-or-complete do--tab-force)
  :functions company-complete-common
  :preface
  (declare-function yas-reload-all nil)

  (defun do--check-expansion ()
	(save-excursion
	  (if (looking-at "\\_>") t ;; end of a string
		(backward-char 1)
		(if (looking-at "\\.") t ;; the last char is '.'
		  (backward-char 1)
		  (if (looking-at "->") t ;; the last char is '->'
			nil)))))
  :init
  (general-define-key
   :states 'insert
   "<tab>" #'do--tab-indent-or-complete
   "M-<tab>" #'do--tab-force)

  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)

  (defun do--tab-force ()
	(interactive)
	(if (not (equal major-mode 'org-mode))
		(tab-to-tab-stop)
	  (org-cycle)))

  (defun do--tab-indent-or-complete ()
	(interactive)

	(if (minibufferp)
		(minibuffer-complete)

	  (if (or (not yas-minor-mode)
			  (null (yas-expand)))
		  (if (do--check-expansion)
			  (company-complete-common)
			(do--tab-force)))))

  (yas-reload-all))

  (use-package auto-yasnippet
	:ensure t
	:after (:all yasnippet evil)
	:commands (aya-create aya-expand)
	:functions evil-insert-state
	:preface
	:init
	(general-define-key
	 :states 'normal
	 "l ~" #'aya-create)
	:config
	(general-define-key
	 :states 'normal
	 "l <tab>" #'(lambda () (interactive) (evil-insert-state)(aya-expand)))

	(general-define-key
	 :states 'insert
	 "C-l ~" #'aya-create
	 "C-l <tab>" #'aya-expand))
