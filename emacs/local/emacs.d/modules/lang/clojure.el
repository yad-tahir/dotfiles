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
   "ls" 'cider-toggle-trace-var
   "lS" 'cider-toggle-trace-ns
   "la" 'cider-apropos-select
   "lA" 'cider-apropos-documentation-select
   "lh" 'cider-doc
   "lr" 'cider-grimoire
   "lw" 'cider-grimoire-web
   "lj" 'cider-javadoc)

  (general-define-key
   :keymaps 'clojure-mode-map
   :states 'normal
   ;; "le" '(:ignore t :which-key "eval")
   "les" 'cider-jack-in
   "leS" 'cider-connect
   "leb" 'cider-eval-buffer
   "lef" 'cider-eval-defun-at-point
   "ler" 'cider-eval-region
   "lee" 'cider-eval-sexp-at-point
   "leE" 'cider-eval-last-sexp
   "lel" 'cider-eval-file
   "leL" 'cider-eval-all-files

   "lln" 'cider-ns-reload
   "llN" 'cider-load-all-project-ns
   "llb" 'cider-load-buffer
   "llf" 'cider-load-file
   "llF" 'cider-load-all-files

   ;; "lh" '(:ignore t :which-key "help")
   "lha" 'cider-apropos
   "lhA" 'cider-apropos-documentation
   "lhh" 'cider-doc
   "lhg" 'cider-grimoire
   "lhG" 'cider-grimoire-web
   "lhj" 'cider-javadoc
   "lhn" 'cider-browse-ns

   ;; "ld" '(:ignore t :which-key "debug")
   "ldf" 'cider-debug-defun-at-point
   "lde" 'cider-enlighten-mode
   "ldi" 'cider-inspect-defun-at-point
   ;; "ldI" 'cider-inspect

   "lr" 'cider-refresh
   ;; "lq" 'cider-quit
   ;; "=" 'cider-format-buffer ;; it is not that great!

   ;; "lg" '(:ignore t :which-key "go")
   "lge" 'cider-jump-to-compilation-error
   "lgs" 'cider-stacktrace-jump
   "lgv" 'cider-find-var
   "gd" 'cider-find-var
   "lgn" 'cider-find-ns
   "lgk" 'cider-find-keyword
   "lgd" 'cider-find-dwim
   "lg:" 'cider-find-property

   ;; "lm" '(:ignore t :which-key "macro")
   "lme" 'cider-macroexpand-1
   "lma" 'cider-macroexpand-all
   "lmz" 'cider-macroexpand-undo
   "lmr" 'cider-macroexpand-again
   "lmm" 'cider-macroexpand-mode)

  (general-define-key
   :keymaps 'cider-inspector-mode-map
   "SPC"   'nil
   "lf" 'cider-inspect-defun-at-point
   "le" 'cider-inspect-expr
   "ll" 'cider-inspect-last-sexp
   "lp" 'cider-inspector-pop
   "ln" 'cider-inspector-next-page
   "lh" 'cider-inspector-prev-page
   "lr" 'cider-inspector-refresh)

  (general-define-key
   :keymaps 'cider-test-report-mode-map
   ;; "l"   'nil
   "ln" 'cider-test-next-result
   "lh" 'cider-test-previous-result
   "lj" 'cider-test-jump
   "ld" 'cider-test-ediff
   "ls" 'cider-test-stacktrace
   "SPC lq" 'cider-popup-buffer-quit
   "lR" 'cider-test-rerun-tests
   "lr" 'cider-test-run-test
   "ln" 'cider-test-run-ns-tests)

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
            #'(lambda ()
                (add-hook 'after-save-hook
                          #'(lambda ()
                              (cider-eval-buffer))
                          nil t ))))


(use-package flycheck-clojure
  :ensure t
  :after (:all flyceck clojure-mode)
  :hook ((flycheck-mode . flycheck-clojure-setup)))


(provide 'do-clojure)
