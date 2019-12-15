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

(use-package tramp
  :commands (do-sudo-current-file)
  :preface
  (declare-function do--sudo-find-file nil)
  :init
  (general-define-key
   :keymaps 'override
   :prefix "SPC s"
   :states 'normal
   "u" #'do-sudo-current-file)
  :config
  (setenv "CDPATH" ".:/usr/bin")

  (setq explicit-shell-file-name "/bin/bash"
		password-cache t ; enable password caching
		password-cache-expiry 7200 ;in seconds
		tramp-connection-timeout 200
		tramp-default-method "ssh")

  (connection-local-set-profile-variables
   'remote-bash
   '((explicit-shell-file-name . "/bin/bash")
	 (explicit-bash-args . ("-i"))))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh" :machine "localhost")
   'remote-bash)

  (defun do--sudo-find-file (file-name)
	"Like find file, but opens the file as root."
	(interactive "FSudo Find File: ")
	(let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
	  (find-file tramp-file-name)))


  (defun do-sudo-current-file ()
	(interactive)
	(eval-and-compile
	  (require 's))
	(let ((name (buffer-file-name)))
	  (when (null name)
		(setq name default-directory))
	  (when (not (s-contains? "sudo" name))
		(do--sudo-find-file name))))

  (add-hook 'eshell-mode-hook #'(lambda () (require 'em-tramp))))


(provide 'do-tramp)
