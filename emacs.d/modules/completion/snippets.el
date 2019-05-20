;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

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
