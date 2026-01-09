;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2026

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
  :preface
  (declare-function yas-reload-all nil)
  :config
  (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-global-mode 1)
  (yas-reload-all)
  (advice-add 'do--tab-complete
			  :around
			  #'(lambda (org-fn  &rest args)
				 (unless (yas-expand)
				   (apply org-fn args)))))

  (use-package auto-yasnippet
	:ensure t
	:after (:all yasnippet evil)
	:commands (aya-create aya-expand)
	:functions evil-insert-state
	:preface
	:init
	(general-define-key
	 :states 'normal
	 "SPC l~" #'aya-create)

	:config
	(general-define-key
	 :states 'normal
	 "SPC l TAB" #'(lambda () (interactive) (evil-insert-state)(aya-expand)))
	(general-define-key
	 :states 'insert
	 "C-l ~" #'aya-create
	 "C-l TAB" #'aya-expand))


(provide 'do-snippets)
