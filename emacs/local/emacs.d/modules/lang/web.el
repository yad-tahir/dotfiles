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

(use-package web-mode
  :ensure t
  :commands (web-mode)
  :preface
  ;; To silent compiler warnings
  (declare-function do--web-init nil)

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
  (setq web-mode-enable-current-element-highlight t)
  ;; Hooks
  ;; (defun do--web-init()
  ;;	"Called whenever Web mode is loaded."
	;;Adjust company's settings
	;; (eval-when-compile
	;;   (require 'company))
	;; (with-eval-after-load 'company
	;;   (set (make-local-variable 'company-backends)
	;;	   (add-to-list 'company-backends
	;;					'(;; Highest priority
	;;					  company-semantic
	;;					  company-capf
	;;					  company-files
	;;					  ;; Current mode
	;;					  ;; NONE
	;;					  ;; Lowest priority - keep the ordering
	;;					  company-keywords
	;;					  company-dabbrev-code
	;;					  company-dabbrev
	;;					  company-ispell)
	;;					nil))))
  ;; (add-hook 'web-mode-hook #'do--web-init()))

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :hook ((web-mode . emmet-mode)
		 (sgml-mode . emmet-mode))
  :config
  (general-define-key
   :keymaps 'emmet-mode-keymap
   :states '(normal insert)
   "<M-tab>" 'emmet-expand-yas))

;; (use-package impatient-mode
;;   :ensure t
;;   :hook ((html-mode . impatient-mode)
;;		 (web-mode . impatient-mode))
;;   :config
;;   (httpd-start)

;;   (defun do-web-open-browser ()
;;	"Opens a browser to render the current buffer with the impatient mode."

;;	(interactive)

;;	;; Check if the server is alive
;;	(unless (process-status "httpd")
;;	  (httpd-start))

;;	;; Check if impatient mode is up and ready
;;	(unless impatient-mode
;;	  (impatient-mode))

;;	(let ((url (format "http://127.0.0.1:%d/imp/live/%s/"
;;					   httpd-port
;;					   (url-hexify-string (buffer-name)))))
;;	  (browse-url url))))

(provide 'do-web)
