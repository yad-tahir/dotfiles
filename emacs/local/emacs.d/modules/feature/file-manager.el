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

(use-package dired
  :commands (do-file-manager dired)
  :functions (do--dired-simultaneous-find)
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC f" 'find-file
   "SPC F" 'dired)

  (general-define-key
   :states 'normal
   "gf" 'dired-jump
   "gF" 'dired-jump-other-window)

  (general-define-key
   :states 'visual
   "gf" nil
   "gF" nil)

  (defun do-file-manager (&optional path)
    (interactive)
    (do-make-frame "file-manager")
    (if (null path)
        (dired default-directory)
      (dired path)))

  :config
  ;; Remove compiler warnings
  (eval-when-compile
    (declare-function dired-get-marked-files nil)
    (declare-function dired-get-filename nil)
    (declare-function dired-find-alternate-file nil)
    (declare-function dired-find-file nil)
    (declare-function dired-current-directory nil)
    (declare-function dired-goto-file nil)
    (declare-function dired-goto-subdir nil)
    (declare-function dired-sort-toggle-or-edit nil)
    (require 'wdired))

  (defun do--dired-simultaneous-find (file-list frame)
    "Find all marked files in the dired mode.
        If FRAME is t, create a separate frame for each file."
    (while (not (null file-list))
      (let* ((file (car file-list)))
        (if frame
            (progn
              (do-make-frame)
              (find-file file))
          (find-file-noselect file)))
      (setq file-list (cdr file-list))))

  (defun do-dired-find-marked-files-noframe ()
    "Find all marked files in the dired buffer."
    (interactive)
    (do--dired-simultaneous-find (dired-get-marked-files) nil))

  (defun do-dired-find-marked-files ()
    "Find all marked files in the dired buffer. Display each file in a
separate frame."
    (interactive)
    (do--dired-simultaneous-find (dired-get-marked-files) t))

  (defun do-dired-find-file ()
    "Get the file under POINT. Open it in a new buffer if it is not a directory."
    (interactive)
    (if (file-directory-p (dired-get-filename))
        (dired-find-alternate-file)
      (dired-find-file)))

  (defun do-dired-up-directory ()
    "Run Dired on parent directory of current directory without creating a new
  buffer."
    (interactive)
    (let* ((dir (dired-current-directory))
           (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
          ;; Only try dired-goto-subdir if buffer has more than one dir.
          (and (cdr dired-subdir-alist)
               (dired-goto-subdir up))
          (progn
            (kill-buffer (current-buffer))
            (dired up)
            (dired-goto-file dir)))))

  (setq wdired-allow-to-change-permissions t
        wdired-create-parent-directories t
        wdired-allow-to-redirect-links t
        dired-auto-revert-buffer t
        dired-listing-switches "-alih"
        dired-no-confirm t)

  ;; (add-hook 'dired-mode-hook
  ;;		  (lambda ()
  ;;			(dired-sort-toggle-or-edit)))  ; Apply sorting

  ;; Auto-refresh dired on file change
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  (general-define-key
   :keymaps 'wdired-mode-map
   :states 'normal
   "w" 'evil-forward-word-begin
   "W" 'evil-forward-WORD-begin
   "b" 'evil-backward-word-begin
   "B" 'evil-backward-WORD-begin
   "SPC lw" 'wdired-finish-edit
   "SPC lq" 'wdired-abort-changes)

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'visual
   "t" 'evil-next-line
   "c" 'evil-previous-line
   ;; Mark
   "m" 'dired-mark
   "u" 'dired-unmark)

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   ";" 'nil
   "q" 'nil
   "s" 'nil

   ;; Navigation
   "t" 'dired-next-line
   "c" 'dired-previous-line
   "<RET>" 'dired-find-file
   "<M-RET>" 'dired-find-file-other-window
   "<C-RET>" 'dired-find-file-other-window
   "DEL" 'dired-up-directory
   "{" 'dired-prev-dirline
   "}" 'dired-next-dirline
   "<" 'dired-prev-dirline
   ">" 'dired-next-dirline
   [remap next-line] 'dired-next-line
   [remap previous-line] 'dired-previous-line

   ;; Undo
   "z" 'dired-undo
   [remap undo] 'dired-undo
   [remap advertised-undo] 'dired-undo

   ;; Search
   "l/" 'nil
   "l/f" 'find-grep-dired
   "l/g" 'dired-do-find-regexp
   "l/r" 'dired-do-find-regexp-and-replace
   "l/*" 'dired-do-find-regexp

   ;; Mark
   "m" 'dired-mark
   "u" 'dired-unmark

   "l*" 'nil
   "l*x" 'dired-mark-executables
   "l*d" 'dired-mark-directories
   "l*@" 'dired-mark-symlinks
   "l*f" 'dired-mark-files-regexp
   "l*s" 'dired-mark-subdir-files
   "l*?" 'dired-unmark-all-files
   "l*!" 'dired-unmark-all-marks
   "l*t" 'dired-toggle-marks
   "l*g" 'dired-mark-files-containing-regexp
   "l*~" 'dired-flag-garbage-files
   "l* <RET>" 'do-dired-find-marked-files-noframe
   "l* <M-RET>" 'do-dired-find-marked-files

   ;; Operations
   "ls" 'dired-sort-toggle-or-edit
   "le" 'wdired-change-to-wdired-mode
   "lf" 'find-file
   "l+" 'dired-create-directory
   "l=" 'dired-diff

   "+" 'dired-create-directory
   "F" 'find-file
   "a" 'dired-toggle-read-only
   "A" 'dired-toggle-read-only
   "i" 'dired-toggle-read-only
   "I" 'dired-toggle-read-only
   "U" 'dired-toggle-read-only)

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "Y" 'dired-do-copy
   "D" 'dired-do-delete
   "M" 'dired-do-rename
   "L" 'dired-do-symlink
   "!" 'dired-do-shell-command
   "&" 'dired-do-async-shell-command

   "ly" 'dired-do-copy
   "ld" 'dired-do-delete
   "lm" 'dired-do-rename
   "ll" 'dired-do-symlink
   "lL" 'dired-do-hardlink
   "lo" 'dired-do-chown
   "lO" 'dired-do-chgrp
   "lx" 'dired-do-chmod
   "l!" 'dired-do-shell-command
   "l&" 'dired-do-async-shell-command
   "lz" 'dired-do-compress-to
   "lZ" 'dired-do-compress
   "lt" 'dired-do-touch
   "lv" 'do-dired-hidden-toggle)

  (defmacro do--dired-visual-marking (func)
    "Marks the selected region before executing FUNC."

    (call-interactively (quote dired-mark))
    `(call-interactively (quote ,func)))

  (defun do-dired-hidden-toggle ()
    "Toggle the display of hidden files in Dired."
    (interactive)
    (if (string-match-p "a" dired-actual-switches)
        (prog1
            (setq dired-actual-switches (replace-regexp-in-string "a" "" dired-actual-switches))
          (message "Hiding hidden files"))
      (prog1
          (setq dired-actual-switches (concat dired-actual-switches "a"))
        (message "Showing hidden files")))
    (revert-buffer))

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'visual
   "m" 'dired-mark
   "Y" '(lambda ()(interactive)(do--dired-visual-marking dired-do-copy))
   "D" '(lambda ()(interactive)(do--dired-visual-marking dired-do-delete))
   "M" '(lambda ()(interactive)(do--dired-visual-marking dired-do-rename))
   "L" '(lambda ()(interactive)(do--dired-visual-marking dired-do-symlink))
   "O" '(lambda ()(interactive)(do--dired-visual-marking dired-do-chown))
   "!" '(lambda ()(interactive)(do--dired-visual-marking dired-do-shell-command))
   "&" '(lambda ()(interactive)(do--dired-visual-marking dired-do-async-shell-command))

   "ly" '(lambda ()(interactive)(do--dired-visual-marking dired-do-copy))
   "ld" '(lambda ()(interactive)(do--dired-visual-marking dired-do-delete))
   "lm" '(lambda ()(interactive)(do--dired-visual-marking dired-do-rename))
   "ll" '(lambda ()(interactive)(do--dired-visual-marking dired-do-symlink))
   "lL" '(lambda ()(interactive)(do--dired-visual-marking dired-do-hardlink))
   "lo" '(lambda ()(interactive)(do--dired-visual-marking dired-do-chown))
   "lO" '(lambda ()(interactive)(do--dired-visual-marking dired-do-chgrp))
   "lx" '(lambda ()(interactive)(do--dired-visual-marking dired-do-chmod))
   "l!" '(lambda ()(interactive)(do--dired-visual-marking dired-do-shell-command))
   "l&" '(lambda ()(interactive)(do--dired-visual-marking dired-do-async-shell-command))
   "lz" '(lambda ()(interactive)(do--dired-visual-marking dired-do-compress-to))
   "lZ" '(lambda ()(interactive)(do--dired-visual-marking dired-do-compress))
   "lt" '(lambda ()(interactive)(do--dired-visual-marking dired-do-touch))))

(use-package dired-open
  :ensure t
  :commands (dired-open-xdg)
  :init
  (with-eval-after-load 'dired
    (general-define-key
     :keymaps 'dired-mode-map
     :states 'normal
     "l <RET>" 'dired-open-xdg)))

(use-package dired-ranger
  ;; :disabled t
  :ensure t
  :commands (dired-ranger-copy dired-ranger-move dired-ranger-paste)
  :after dired
  :init
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "Y" 'dired-ranger-copy
   "M" 'dired-ranger-move
   "P" 'dired-ranger-paste
   "ly" 'dired-ranger-copy
   "lm" 'dired-ranger-move
   "lp" 'dired-ranger-paste
   "lY" 'dired-do-copy
   "lM" 'dired-do-rename)

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'visual
   "Y" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-copy))
   "M" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-move))
   "P" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-paste))
   "ly" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-copy))
   "lm" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-move))
   "lp" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-paste))
   "lY" '(lambda () (interactive) (do--dired-visual-marking dired-do-copy))
   "lM" '(lambda () (interactive) (do--dired-visual-marking dired-do-rename))))

(use-package dired-collapse
  ;; :disabled t
  :ensure t
  :commands (dired-collapse-mode)
  ;; :hook ((dired-mode . dired-collapse-mode))
  :init
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "lc" 'dired-collapse-mode))

(use-package dired-subtree
  :disabled t
  :ensure t
  :commands (dired-subtree-insert
             dired-subtree-remove
             dired-subtree-beginning
             dired-subtree-end)
  :init
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "TAB" 'dired-subtree-insert
   "<M-tab>" 'dired-subtree-remove
   "[" 'dired-subtree-beginning
   "]" 'dired-subtree-end
   "l*}" 'dired-subtree-mark-subtree))

(use-package openwith
  :ensure t
  :hook ((org-mode . openwith-mode)
         (dired-mode . openwith-mode))
  :after (:any dired org)
  :config
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file))
                                ("\\.cr3\\'" "gimp" (file))
                                ("\\.cr2\\'" "gimp" (file))
                                ("\\.mp4\\'" "mpv" (file)))))

;; For large file
(use-package vlf
  :defer t
  :ensure t
  :config
  (require 'vlf-setup))


(provide 'do-file-manager)
