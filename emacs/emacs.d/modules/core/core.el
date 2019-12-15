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

(defconst emacs-start-time (current-time))

;; GC optimizations
(defvar original-gc-cons-threshold gc-cons-threshold)
(defvar original-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold 402653184
	  gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
		  '(lambda () (setq gc-cons-threshold original-gc-cons-threshold
							gc-cons-percentage original-gc-cons-percentage)) t)

;; Bootstrap 'use-package'
(eval-and-compile
  (require 'package)
  (setq package--init-file-ensured t
		package-enable-at-startup nil
		package-user-dir (concat user-emacs-directory "/packages"))
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  ;; (package-refresh-contents)

  (unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
  ;; Configure 'use-package'
  (setq use-package-compute-statistics t
		use-package-verbose nil))

;; Module manager
(defun do-modules-load (&rest modules)
  "Goes through each module in the list MODULES, one by one, and loads it to Emacs.

A module is a regular elisp file that exists in the 'modules' directory.

MODULES must hold module paths. Each path must be relative to
the 'modules' directory and without the '.el' extension.

This function automatically byte compiles module files as necessary. Modules are
 compiled and loaded based on their order in MODULES."

  (let* ((d (concat user-emacs-directory "modules/"))
		 (default-directory d))
	;; Add to load path
	(normal-top-level-add-subdirs-to-load-path)

	(dolist (m modules)
	  (byte-recompile-file (concat d m ".el") nil 0 t))))

(defun display-startup-echo-area-message ()
  (message "Loading done in %.3fs"
		   (float-time
			(time-subtract (current-time) emacs-start-time))))

;; Core packages
(use-package general
  :ensure t
  :demand t)

(use-package auto-package-update
  :disabled t
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package benchmark-init
  :disabled t
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


(provide 'do-core)
