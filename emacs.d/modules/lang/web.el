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
