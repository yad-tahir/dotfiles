;;; -*- lexical-binding: t; -*-

(defconst emacs-start-time (current-time))
(setq package--init-file-ensured t)

;; GC optimizations
(defvar original-gc-cons-threshold gc-cons-threshold)
(defvar original-gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold 402653184
	  gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
		  '(lambda () (setq gc-cons-threshold original-gc-cons-threshold
							gc-cons-percentage original-gc-cons-percentage)) t)


(require 'package)
(setq package-enable-at-startup nil
	  package-user-dir (concat user-emacs-directory "/packages"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; (package-refresh-contents)

(eval-and-compile
  ;; Bootstrap 'use-package'
  (unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
  ;; Configure 'use-package'
  (setq use-package-compute-statistics t
		use-package-verbose nil))

(use-package auto-package-update
  :disabled t
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package benchmark-init
  :ensure t
  :disabled t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defvar do-modules-list '())

(defun do-modules (&rest modules)
  "Load the given modules"
  (setq do-modules-list modules))

(defun do-modules-update-bootstrap ()
  (interactive)
  (cd "~/.emacs.d")
  (let ((generated-bootstrap-file
		 (expand-file-name "modules-bootstrap.el")))
	(with-current-buffer (find-file-noselect
						  generated-bootstrap-file)
	  (erase-buffer)
	  (insert ";;; -*- lexical-binding: t; -*-")
	  ;; (insert "\n(eval-and-compile ")
	  (dolist (m do-modules-list)
		(insert-file-contents (concat "~/.emacs.d/modules/" m ".el"))
		(goto-char (point-max)))
	  ;; (insert ")")
	  (save-buffer)
	  (kill-buffer))
	(byte-recompile-file "~/.emacs.d/modules-bootstrap.el" t)
	))

(defun do-modules-bootstrap-load (&optional new-bootstrap)
  (when new-bootstrap
	(do-modules-update-bootstrap))
  (load "~/.emacs.d/modules-bootstrap"))

(defun do-modules-load (&optional compile)
  "Load modules in 'do-modules-list'.

The first argument, compile, indicates whether to recompile the modules."

  (let* ((d (concat user-emacs-directory "modules/"))
		 (default-directory d))
	;; Add to load path
	(normal-top-level-add-subdirs-to-load-path)
	;; Recompile if it is needed it
	;; (when compile
	;;   (byte-recompile-directory d 0))
	;; Go through the modules and load them one by one
	(dolist (m do-modules-list)
	  (if compile
		  (byte-recompile-file (concat d m ".el") nil 0 t)
		;; @TODO: Change to require
		(load (concat d m))))))

(use-package general
  :ensure t
  :demand t)

(defun display-startup-echo-area-message ()
  (message "Loading done in %.3fs"
		   (float-time
			(time-subtract (current-time) emacs-start-time))))

(provide 'core)
