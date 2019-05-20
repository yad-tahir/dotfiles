;;; -*- lexical-binding: t; -*-

(use-package go-mode
  :ensure t
  :defer t
  :commands (go-mode)
  :preface
  (declare-function do--go-mode-init nil)
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (setenv "PATH" (concat (getenv "PATH") ":/home/yad/.local/go/bin"))
  (setenv "GOPATH" "/home/yad/.local/go")
  (add-to-list 'exec-path "/home/yad/.local/go/bin")
  :config
  (defun do--go-mode-init ()
	(add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
	(setq gofmt-command "goimports")                ; gofmt uses invokes goimports
	(if (not (string-match "go" compile-command))   ; set compile command default
		(set (make-local-variable 'compile-command)
			 "go build -v && go test -v && go vet"))
	)
  (add-hook 'go-mode-hook #'do--go-mode-init) )

(use-package go-dlv
  :ensure t
  :after (go-mode)
  :commands (dlv dlv-current-func)
  )


(use-package company-go
  :after (:all go-mode company)
  :ensure t
  :config
  (add-hook 'go-mode-hook (lambda ()
							(set (make-local-variable 'company-backends)
								 (append company-backends
										 '((company-go
											:with company-dabbrev-code
											:with company-keywords)))))))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-env "GOPATH"))


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


