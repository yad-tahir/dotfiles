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

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :interpreter "go"
  :preface
  (declare-function do--go-mode-init nil)
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (setenv "PATH" (concat (getenv "PATH") ":/home/yad/go/bin"))
  (setenv "GOPATH" "/home/yad/go")
  (add-to-list 'exec-path "/home/yad/go/bin")
  :config
  (defun do--go-mode-init ()
	;; (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
	;; (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
	(if (not (string-match "go" compile-command))   ; set compile command default
		(set (make-local-variable 'compile-command)
			 "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook #'do--go-mode-init))

;; (use-package go-dlv
;;   :ensure t
;;   :after (go-mode)
;;   :commands (dlv dlv-current-func)
;;   )


;; (use-package company-go
;;   :after (:all go-mode company)
;;   :ensure t
;;   :config
;;   (add-hook 'go-mode-hook (lambda ()
;;							(set (make-local-variable 'company-backends)
;;								 (append company-backends
;;										 '((company-go
;;											:with company-dabbrev-code
;;											:with company-keywords)))))))

;; (use-package flycheck-gometalinter
;;   :after (go-mode flycheck)
;;   :hook ((flycheck-mode . flycheck-gometalinter-setup))
;;   :ensure t
;;   :config
;; ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
;; ;; (setq flycheck-gometalinter-vendor t)
;; ;; ;; only show errors
;; ;; (setq flycheck-gometalinter-errors-only t)
;; ;; only run fast linters
;; ;; (setq flycheck-gometalinter-fast t)
;; ;; use in tests files
;; ;; (setq flycheck-gometalinter-test t)
;; ;; disable linters
;; ;; (setq flycheck-gometalinter-disable-linters '("megacheck" "deadcode"))
;; ;; Only enable selected linters
;; (setq flycheck-gometalinter-disable-all t)
;; (setq flycheck-gometalinter-enable-linters '("golint"))
;; ;; Set different deadline (default: 5s)
;; (setq flycheck-gometalinter-deadline "10s")
;; ;; Use a gometalinter configuration file (default: nil)
;; ;; (setq flycheck-gometalinter-config "/path/to/gometalinter-config.json")
;;   )


(provide 'do-go)
