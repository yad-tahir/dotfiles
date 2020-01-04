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

(use-package eshell
  :after (evil)
  :commands (eshell)
  :ensure t
  :preface
  (declare-function do-eshell-new nil)
  (declare-function do--eshell-init nil)
  (declare-function eshell nil)
  :init
  (general-define-key
   :prefix "SPC"
   :keymaps 'override
   :states '(normal visual)
   "RET" #'do-eshell-new)

;;;###autoload
  (defun do-eshell-new()
	"Open a new instance of eshell."
	(interactive)
	;; (do-make-frame)
	(eshell 'N))

  :config
  ;; Remove compiler warnings
  (eval-when-compile
	(require 'eshell)
	(require 'em-term)
	(require 'em-hist)
	(require 'em-glob))

  (setq eshell-prefer-lisp-functions 'nil
		eshell-scroll-to-bottom-on-input 'all
		eshell-scroll-to-bottom-on-output 'all
		eshell-kill-processes-on-exit t
		eshell-save-history-on-exit t
		eshell-error-if-no-glob t)

  (with-eval-after-load 'em-term
	(add-to-list 'eshell-visual-commands "htop")
	(add-to-list 'eshell-visual-commands "top")
	(add-to-list 'eshell-visual-commands "glances")
	(add-to-list 'eshell-visual-commands "ssh")
	(add-to-list 'eshell-visual-commands "tail")
	(add-to-list 'eshell-visual-commands "pulsemixer"))

  (custom-set-faces
   `(term-color-black ((t (:foreground ,chocolate-theme-shadow+3 :background nil))))
   `(term-color-red ((t (:foreground ,chocolate-theme-highlight ))))
   `(term-color-green ((t (:foreground ,chocolate-theme-element ))))
   `(term-color-yellow ((t (:foreground ,chocolate-theme-highlight+2))))
   `(term-color-blue ((t (:foreground ,chocolate-theme-element+4))))
   `(term-color-magenta ((t (:foreground ,chocolate-theme-element+4))))
   `(term-color-cyan ((t (:foreground ,chocolate-theme-element+4))))
   `(term-color-white ((t (:foreground ,chocolate-theme-white))))
   `(term-default-fg-color ((t (:inherit term-color-white))))
   `(term-default-bg-color ((t (:inherit term-color-black)))))

  (defun eshell/clear ()
	"clear the eshell buffer."
	(interactive)
	(let ((inhibit-read-only t))
	  (erase-buffer)))

  (defun do--eshell-init ()
	(with-eval-after-load 'company
	  (company-mode 1)))
  (add-hook 'eshell-mode-hook #'do--eshell-init))

(use-package fish-completion
  :after (eshell)
  :ensure t
  :hook ((eshell-mode . fish-completion-mode))
  :config
  (with-eval-after-load 'em-term
	;; Basic eShell key bindings should be in this minor mode as the
	;; implementation of eshell-mode-map really sucks!
	;; (general-define-key
	;;  :keymaps 'eshell-mode-map
	;;  :states 'insert
	;;  "<tab>" 'completion-at-point
	;;  "C-q" 'eshell-interrupt-process
	;;  "C-c" 'eshell-previous-input
	;;  "C-t" 'eshell-next-input)
	))

;; (use-package eshell-fixed-prompt
;;   :ensure t
;;   :hook ((eshell-mode .  eshell-fixed-prompt-mode)))


;; (use-package eshell-fringe-status
;;   :hook ((eshell-mode . eshell-fringe-status-mode)))

(use-package eterm-256color
  :ensure t
  :hook ((term-mode . eterm-256color-mode)))


(use-package exec-path-from-shell
  :defer 20
  :ensure t
  :config
  (setq exec-path-from-shell-arguments (list "-l")
		exec-path-from-shell-shell-name "bash"
		exec-path-from-shell-check-startup-files nil)

		(add-to-list 'exec-path-from-shell-variables "SHELL")
		(add-to-list 'exec-path-from-shell-variables "GOPATH")
		(add-to-list 'exec-path-from-shell-variables "GNUPGHOME")
		(add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")

		(exec-path-from-shell-initialize))


(provide 'do-shell)
