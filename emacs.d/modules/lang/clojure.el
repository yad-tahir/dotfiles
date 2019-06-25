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

(use-package clojure-mode
  :commands (clojure-mode)
  :defer t
  :ensure t)

(use-package cider
  :after(clojure-mode)
  :ensure t
  :preface
  (declare-function do--cider-window nil)
  :config
  (setq cider-use-tooltips t
		cider-show-error-buffer 'only-in-repl
		cider-repl-display-help-banner nil
		cider-repl-display-in-current-window nil)
  (general-define-key
   :keymaps 'cider-repl-mode-map
   :states 'insert
   "C-c" 'cider-repl-previous-input
   "C-t" 'cider-repl-next-input)

  (general-define-key
   :keymaps 'cider-repl-mode-map
   :states 'normal
   "xs" 'cider-toggle-trace-var
   "xS" 'cider-toggle-trace-ns
   "xa" 'cider-apropos-select
   "xA" 'cider-apropos-documentation-select
   "xh" 'cider-doc
   "xr" 'cider-grimoire
   "xw" 'cider-grimoire-web
   "xj" 'cider-javadoc)

  (general-define-key
   :keymaps 'clojure-mode-map
   :states 'normal
   "xe" '(:ignore t :which-key "eval")
   "xes" 'cider-jack-in
   "xeS" 'cider-connect
   "xeb" 'cider-eval-buffer
   "xef" 'cider-eval-defun-at-point
   "xer" 'cider-eval-region
   "xee" 'cider-eval-sexp-at-point
   "xeE" 'cider-eval-last-sexp
   "xel" 'cider-eval-file
   "xeL" 'cider-eval-all-files

   "xl" '(:ignore t :which-key "load")
   "xln" 'cider-ns-reload
   "xlN" 'cider-load-all-project-ns
   "xlb" 'cider-load-buffer
   "xlf" 'cider-load-file
   "xlF" 'cider-load-all-files

   "xh" '(:ignore t :which-key "help")
   "xha" 'cider-apropos
   "xhA" 'cider-apropos-documentation
   "xhh" 'cider-doc
   "xhg" 'cider-grimoire
   "xhG" 'cider-grimoire-web
   "xhj" 'cider-javadoc
   "xhn" 'cider-browse-ns

   "xd" '(:ignore t :which-key "debug")
   "xdf" 'cider-debug-defun-at-point
   "xde" 'cider-enlighten-mode
   "xdi" 'cider-inspect-defun-at-point
   ;; "xdI" 'cider-inspect

   "xr" 'cider-refresh
   ;; "xq" 'cider-quit
   ;; "=" 'cider-format-buffer ;; it is not that great!

   "xg" '(:ignore t :which-key "go")
   "xge" 'cider-jump-to-compilation-error
   "xgs" 'cider-stacktrace-jump
   "xgv" 'cider-find-var
   "gd" 'cider-find-var
   "xgn" 'cider-find-ns
   "xgk" 'cider-find-keyword
   "xgd" 'cider-find-dwim
   "xg:" 'cider-find-property

   "xm" '(:ignore t :which-key "macro")
   "xme" 'cider-macroexpand-1
   "xma" 'cider-macroexpand-all
   "xmz" 'cider-macroexpand-undo
   "xmr" 'cider-macroexpand-again
   "xmm" 'cider-macroexpand-mode)

  (general-define-key
   :keymaps 'cider-inspector-mode-map
   "x"   'nil
   "xf" 'cider-inspect-defun-at-point
   "xe" 'cider-inspect-expr
   "xl" 'cider-inspect-last-sexp
   "xp" 'cider-inspector-pop
   "xn" 'cider-inspector-next-page
   "xh" 'cider-inspector-prev-page
   "xr" 'cider-inspector-refresh)

  (general-define-key
   :keymaps 'cider-test-report-mode-map
   "x"   'nil
   "xn" 'cider-test-next-result
   "xh" 'cider-test-previous-result
   "xj" 'cider-test-jump
   "xd" 'cider-test-ediff
   "xs" 'cider-test-stacktrace
   "xq" 'cider-popup-buffer-quit
   "xR" 'cider-test-rerun-tests
   "xr" 'cider-test-run-test
   "xn" 'cider-test-run-ns-tests)

  (set-face-attribute 'cider-fringe-good-face
					  nil :foreground chocolate-theme-shadow+3
					  :background chocolate-theme-shadow+3)

  (defun do--cider-window(org-fun &rest args)
	"Creates a new frame before switching to a new cider buffer"
	(do-make-frame)
	(with-eval-after-load 'company
	  (company-mode 1))
	(apply org-fun args))
  (advice-add 'cider-repl-init :around #'do--cider-window)
  (advice-add 'cider-popup-buffer :around #'do--cider-window)

  (add-hook 'clojure-mode-hook
			'(lambda ()
			   (add-hook 'after-save-hook
						 '(lambda ()
							(cider-eval-buffer))
						 nil t ))))


(use-package flycheck-clojure
  :ensure t
  :after (:all flyceck clojure-mode)
  :hook ((flycheck-mode . flycheck-clojure-setup)))
