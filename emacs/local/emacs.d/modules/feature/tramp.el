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


(use-package s
  :ensure t
  )

(use-package tramp
  :commands (do-sudo-current-file do-vault-home)

  :preface
  (declare-function do--sudo-find-file nil)
  (declare-function do-vault-home nil)

  :init
  (general-define-key
   :keymaps 'override
   :prefix "SPC s"
   :states '(normal visual)
   "u" 'do-sudo-current-file
   "v" 'do-vault-home)

  (setq	password-cache t ; enable password caching
        password-cache-expiry 7200 ;in seconds
        tramp-connection-timeout 200
        tramp-default-method "ssh"
        tramp-histfile-override (expand-file-name ".tramp_history" user-emacs-directory))

  :config
  (defun do-vault-home ()
    (interactive)
    (find-file (concat "/ssh:vault.home:/home/" user-login-name "/")))

  ;; (connection-local-set-profile-variables
  ;;  'remote-bash
  ;;  '((explicit-shell-file-name . "/bin/bash")
  ;;	 (explicit-bash-args . ("-i"))))
  ;; (connection-local-set-profiles
  ;;  '(:application tramp :protocol "ssh" :machine "localhost")
  ;;  'remote-bash)

  (defun do--sudo-find-file (file-name)
    "Like find file, but opens the file as root."
    (eval-and-compile
      (require 'tramp)
      (require 's))
    (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
      ;; We cannot rely on the current char in the dired mode; when tramp is used
      ;; with dired, the dired buffer will include extra characters, highlighting
      ;; the URI of the tramp destination.
      (if (eq major-mode 'dired-mode)
          (let ((pos-name (file-name-nondirectory (dired-get-file-for-visit))))
            (dired-move-to-filename)
            (find-file tramp-file-name)
            (search-forward pos-name nil t)
            (dired-move-to-filename))
        (let ((pos (point))
              (w-start (window-start)))
          (find-file tramp-file-name)
          (goto-char pos) ; restore the original position
          ;; Restore the scroll position so the buffer doesn't "jump"
          (set-window-start (selected-window) w-start)))))

  (defun do--unsudo-find-file (name)
    (eval-and-compile
      (require 'tramp)
      (require 's))
    ;; We cannot rely on the current char in the dired mode; when tramp is used
    ;; with dired, the dired buffer will include extra characters, highlighting
    ;; the URI of the tramp destination.
    (if (eq major-mode 'dired-mode)
        (let ((pos-name (file-name-nondirectory (dired-get-file-for-visit))))
          (dired-move-to-filename)
          (find-file (s-replace ":" "" (nth 1 (s-slice-at ":/" name))))
          (search-forward pos-name nil t)
          (dired-move-to-filename))
      (let ((pos (point))
            (w-start (window-start)))
        (find-file (s-replace ":" "" (nth 1 (s-slice-at ":/" name))))
        (goto-char pos) ; restore the original position
        ;; Restore the scroll position so the buffer doesn't "jump"
        (set-window-start (selected-window) w-start))))

  (eval-when-compile
    (use-package s
      :ensure t))

  (defun do-sudo-current-file ()
    (interactive)
    (eval-and-compile
      (require 's))
    (let ((name (or (buffer-file-name)
                    default-directory)))
      (if (not (s-contains? "sudo" name))
          (do--sudo-find-file name)
        (do--unsudo-find-file name))))

  (with-eval-after-load "tramp"
    (setq vc-ignore-dir-regexp
          (rx (seq bos
                   (or (seq (any "/\\") (any "/\\")
                            (one-or-more (not (any "/\\")))
                            (any "/\\"))
                       (seq "/" (or "net" "afs" "...") "/")
                       ;; Ignore all tramp paths.
                       (seq "/"
                            (eval (cons 'or (mapcar #'car tramp-methods)))
                            ":"
                            (zero-or-more anything)))
                   eos)))))

(provide 'do-tramp)
