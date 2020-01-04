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

(use-package dired
  :commands (do-file-manager dired)
  :functions (do--dired-simultaneous-find)
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC f" 'find-file
   "SPC F" 'dired
   "SPC m" 'bookmark-set)

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
	(interactive)
	"Find all marked files in the dired buffer."
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

  (general-define-key
   :keymaps 'wdired-mode-map
   :states 'normal
   "w" 'evil-forward-word-begin
   "W" 'evil-forward-WORD-begin
   "b" 'evil-backward-word-begin
   "B" 'evil-backward-WORD-begin
   [remap evil-save-and-quit] 'wdired-finish-edit
   [remap evil-quit] 'wdired-abort-changes)

  (general-define-key
   :keymaps 'dired-mode-map
   :states '(normal visual)
   ";" 'nil
   "q" 'nil
   "s" 'nil

   ;; Navigation
   "t" 'dired-next-line
   "c" 'dired-previous-line
   "<RET>" 'dired-find-file
   "<M-RET>" 'dired-find-file-other-window
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
   "SPC l/" 'nil
   "SPC l/f" 'find-grep-dired
   "SPC l/g" 'dired-do-find-regexp
   "SPC l/r" 'dired-do-find-regexp-and-replace
   "SPC l/*" 'dired-do-find-regexp

   ;; Mark
   "m" 'dired-mark
   "u" 'dired-unmark

   "SPC l*" 'nil
   "SPC l*x" 'dired-mark-executables
   "SPC l*d" 'dired-mark-directories
   "SPC l*@" 'dired-mark-symlinks
   "SPC l*f" 'dired-mark-files-regexp
   "SPC l*s" 'dired-mark-subdir-files
   "SPC l*?" 'dired-unmark-all-files
   "SPC l*!" 'dired-unmark-all-marks
   "SPC l*t" 'dired-toggle-marks
   "SPC l*g" 'dired-mark-files-containing-regexp
   "SPC l*~" 'dired-flag-garbage-files
   "SPC l* <RET>" 'do-dired-find-marked-files-noframe
   "SPC l* <M-RET>" 'do-dired-find-marked-files

   ;; Operations
   "SPC ls" 'dired-sort-toggle-or-edit
   "SPC le" 'wdired-change-to-wdired-mode
   "SPC lf" 'find-file
   "SPC l+" 'dired-create-directory
   "SPC l=" 'dired-diff

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

   "SPC ly" 'dired-do-copy
   "SPC ld" 'dired-do-delete
   "SPC lm" 'dired-do-rename
   "SPC ll" 'dired-do-symlink
   "SPC lL" 'dired-do-hardlink
   "SPC lo" 'dired-do-chown
   "SPC lO" 'dired-do-chgrp
   "SPC lx" 'dired-do-chmod
   "SPC l!" 'dired-do-shell-command
   "SPC l&" 'dired-do-async-shell-command
   "SPC lz" 'dired-do-compress-to
   "SPC lZ" 'dired-do-compress
   "SPC lt" 'dired-do-touch)


  (defmacro do--dired-visual-marking (func)
	"Marks the selected region before executing FUNC."

	(call-interactively (quote dired-mark))
	`(call-interactively (quote ,func)))

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

   "SPC ly" '(lambda ()(interactive)(do--dired-visual-marking dired-do-copy))
   "SPC ld" '(lambda ()(interactive)(do--dired-visual-marking dired-do-delete))
   "SPC lm" '(lambda ()(interactive)(do--dired-visual-marking dired-do-rename))
   "SPC ll" '(lambda ()(interactive)(do--dired-visual-marking dired-do-symlink))
   "SPC lL" '(lambda ()(interactive)(do--dired-visual-marking dired-do-hardlink))
   "SPC lo" '(lambda ()(interactive)(do--dired-visual-marking dired-do-chown))
   "SPC lO" '(lambda ()(interactive)(do--dired-visual-marking dired-do-chgrp))
   "SPC lx" '(lambda ()(interactive)(do--dired-visual-marking dired-do-chmod))
   "SPC l!" '(lambda ()(interactive)(do--dired-visual-marking dired-do-shell-command))
   "SPC l&" '(lambda ()(interactive)(do--dired-visual-marking dired-do-async-shell-command))
   "SPC lz" '(lambda ()(interactive)(do--dired-visual-marking dired-do-compress-to))
   "SPC lZ" '(lambda ()(interactive)(do--dired-visual-marking dired-do-compress))
   "SPC lt" '(lambda ()(interactive)(do--dired-visual-marking dired-do-touch))))

(use-package dired-open
  :ensure t
  :commands (dired-open-xdg)
  :init
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "SPC l <RET>" 'dired-open-xdg))

(use-package dired-ranger
  ;; :disabled t
  :ensure t
  :after (dired)
  :config
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "Y" 'dired-ranger-copy
   "M" 'dired-ranger-move
   "P" 'dired-ranger-paste
   "SPC ly" 'dired-ranger-copy
   "SPC lm" 'dired-ranger-move
   "SPC lp" 'dired-ranger-paste
   "SPC lY" 'dired-do-copy
   "SPC lM" 'dired-do-rename)

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'visual
   "Y" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-copy))
   "M" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-move))
   "P" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-paste))
   "SPC ly" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-copy))
   "SPC lm" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-move))
   "SPC lp" '(lambda () (interactive) (do--dired-visual-marking dired-ranger-paste))
   "SPC lY" '(lambda () (interactive) (do--dired-visual-marking dired-do-copy))
   "SPC lM" '(lambda () (interactive) (do--dired-visual-marking dired-do-rename))))

(use-package dired-collapse
  ;; :disabled t
  :ensure t
  :commands (dired-collapse-mode)
  ;; :hook ((dired-mode . dired-collapse-mode))
  :init
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "SPC lc" 'dired-collapse-mode))

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
   "SPC l*}" 'dired-subtree-mark-subtree)
  :config
  (set-face-attribute 'dired-subtree-depth-1-face nil
					  :background chocolate-theme-shadow+1)
  (set-face-attribute 'dired-subtree-depth-2-face nil
					  :background chocolate-theme-shadow+3)
  (set-face-attribute 'dired-subtree-depth-3-face nil
					  :background chocolate-theme-shadow+1)
  (set-face-attribute 'dired-subtree-depth-4-face nil
					  :background chocolate-theme-shadow+3))

(use-package openwith
  :ensure t
  :hook ((org-mode . openwith-mode)
		 (dired-mode . openwith-mode))
  :config
  (setq openwith-associations '(("\\.rar\\'" "file-roller" (file))
								("\\.zip\\'" "file-roller" (file))
								("\\.7z\\'" "file-roller" (file))
								("\\.gz\\'" "file-roller" (file))
								("\\.tar\\'" "file-roller" (file))
								("\\.pdf\\'" "zathura" (file)))))


;; For large file
(use-package vlf
  :defer t
  :ensure t
  :config
  (require 'vlf-setup))


(provide 'do-file-manager)
