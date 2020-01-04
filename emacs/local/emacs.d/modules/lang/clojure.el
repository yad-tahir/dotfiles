;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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
   "SPC ls" 'cider-toggle-trace-var
   "SPC lS" 'cider-toggle-trace-ns
   "SPC la" 'cider-apropos-select
   "SPC lA" 'cider-apropos-documentation-select
   "SPC lh" 'cider-doc
   "SPC lr" 'cider-grimoire
   "SPC lw" 'cider-grimoire-web
   "SPC lj" 'cider-javadoc)

  (general-define-key
   :keymaps 'clojure-mode-map
   :states 'normal
   ;; "SPC le" '(:ignore t :which-key "eval")
   "SPC les" 'cider-jack-in
   "SPC leS" 'cider-connect
   "SPC leb" 'cider-eval-buffer
   "SPC lef" 'cider-eval-defun-at-point
   "SPC ler" 'cider-eval-region
   "SPC lee" 'cider-eval-sexp-at-point
   "SPC leE" 'cider-eval-last-sexp
   "SPC lel" 'cider-eval-file
   "SPC leL" 'cider-eval-all-files

   ;; "SPC ll" '(:ignore t :which-key "load")
   "SPC lln" 'cider-ns-reload
   "SPC llN" 'cider-load-all-project-ns
   "SPC llb" 'cider-load-buffer
   "SPC llf" 'cider-load-file
   "SPC llF" 'cider-load-all-files

   ;; "SPC lh" '(:ignore t :which-key "help")
   "SPC lha" 'cider-apropos
   "SPC lhA" 'cider-apropos-documentation
   "SPC lhh" 'cider-doc
   "SPC lhg" 'cider-grimoire
   "SPC lhG" 'cider-grimoire-web
   "SPC lhj" 'cider-javadoc
   "SPC lhn" 'cider-browse-ns

   ;; "SPC ld" '(:ignore t :which-key "debug")
   "SPC ldf" 'cider-debug-defun-at-point
   "SPC lde" 'cider-enlighten-mode
   "SPC ldi" 'cider-inspect-defun-at-point
   ;; "SPC ldI" 'cider-inspect

   "SPC lr" 'cider-refresh
   ;; "SPC lq" 'cider-quit
   ;; "=" 'cider-format-buffer ;; it is not that great!

   ;; "SPC lg" '(:ignore t :which-key "go")
   "SPC lge" 'cider-jump-to-compilation-error
   "SPC lgs" 'cider-stacktrace-jump
   "SPC lgv" 'cider-find-var
   "gd" 'cider-find-var
   "SPC lgn" 'cider-find-ns
   "SPC lgk" 'cider-find-keyword
   "SPC lgd" 'cider-find-dwim
   "SPC lg:" 'cider-find-property

   ;; "SPC lm" '(:ignore t :which-key "macro")
   "SPC lme" 'cider-macroexpand-1
   "SPC lma" 'cider-macroexpand-all
   "SPC lmz" 'cider-macroexpand-undo
   "SPC lmr" 'cider-macroexpand-again
   "SPC lmm" 'cider-macroexpand-mode)

  (general-define-key
   :keymaps 'cider-inspector-mode-map
   "SPC"   'nil
   "SPC lf" 'cider-inspect-defun-at-point
   "SPC le" 'cider-inspect-expr
   "SPC ll" 'cider-inspect-last-sexp
   "SPC lp" 'cider-inspector-pop
   "SPC ln" 'cider-inspector-next-page
   "SPC lh" 'cider-inspector-prev-page
   "SPC lr" 'cider-inspector-refresh)

  (general-define-key
   :keymaps 'cider-test-report-mode-map
   ;; "SPC l"   'nil
   "SPC ln" 'cider-test-next-result
   "SPC lh" 'cider-test-previous-result
   "SPC lj" 'cider-test-jump
   "SPC ld" 'cider-test-ediff
   "SPC ls" 'cider-test-stacktrace
   [remap evil-quit] 'cider-popup-buffer-quit
   "SPC lR" 'cider-test-rerun-tests
   "SPC lr" 'cider-test-run-test
   "SPC ln" 'cider-test-run-ns-tests)

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


(provide 'do-clojure)
