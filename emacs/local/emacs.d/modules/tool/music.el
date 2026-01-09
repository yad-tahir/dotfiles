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

(use-package simple-mpc
  :ensure t
  :commands (simple-mpc-next
			 simple-mpc-prev
			 simple-mpc-view-current-playlist)
  :functions (do-music-next do-music-previous do--music-playlist-init
							do--music-query-init do--music-setup-process simple-mpc-play-current-line)
  :preface
  (defvar do--music-mpd-init nil)
  (defvar do--music-toggle-state nil)

  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC Am" 'do-music-playlist)

  (defun do--music-setup-process ()
	(unless do--music-mpd-init
	  ;; Start MPD
	  (ignore-errors
		(let ((b (get-buffer "*Messages*")))
		  (shell-command "mpd" b b)))
	  (setq do--music-mpd-init t))
	nil)

  (defun do-music-next ()
	(interactive)
	(do--music-setup-process)
	(setq do--music-toggle-state t)
	(simple-mpc-next)
	(message "Music Next"))

  (defun do-music-previous ()
	(interactive)
	(do--music-setup-process)
	(setq do--music-toggle-state t)
	(simple-mpc-prev)
	(message "Music Previous"))

  (defun do-music-toggle ()
	(interactive)
	(require 'simple-mpc)
	(do--music-setup-process)
	(if do--music-toggle-state
		(progn
		  (start-process-shell-command "mpc" 'nil "/usr/bin/mpc pause")
		  (setq do--music-toggle-state nil)
		  (message "Music Pause"))
	  (progn
		(start-process-shell-command "mpc" 'nil "/usr/bin/mpc play")
		(setq do--music-toggle-state t)
		(message "Music Play"))))

;;;###autoload
  (defun do-music-playlist ()
	(interactive)
	(do--music-setup-process)
	(simple-mpc-view-current-playlist))

  :config
  ;; Sub-functions to encapsulate simple-mpc
  (defun do-music-stop ()
	(interactive)
	(setq do--music-toggle-state t)
	(do--music-setup-process)
	(start-process-shell-command "mpc" 'nil "/usr/bin/mpc stop"))
  (defun do-music-play-selected ()
	(interactive)
	(simple-mpc-play-current-line)
	(setq do--music-toggle-state t)
	(do--music-setup-process))
  (defun do-music-load-playlist ()
	(interactive)
	(call-interactively 'simple-mpc-load-playlist))
  (defun do-music-query ()
	(interactive)
	(call-interactively 'simple-mpc-query))
  (defun do-music-shuffle ()
	(interactive)
	(call-interactively 'simple-mpc-shuffle-current-playlist))
  (defun do-music-seek-forward ()
	(interactive)
	(call-interactively 'simple-mpc-seek-forward))
  (defun do-music-seek-backward ()
	(interactive)
	(call-interactively 'simple-mpc-seek-backward))
  (defun do-music-delete ()
	(interactive)
	(call-interactively 'simple-mpc-delete))
  (defun do-music-clear-playlist ()
	(interactive)
	(call-interactively 'simple-mpc-clear-current-playlist))
  (defun do-music-query-sort ()
	(interactive)
	(call-interactively 'simple-mpc-query-sort))
  (defun do-music-query-quit ()
	(interactive)
	(call-interactively 'simple-mpc-query-quit))
  (defun do-music-query-add ()
	(interactive)
	(call-interactively 'simple-mpc-query-add))

  (setq simple-mpc-mode-map (make-sparse-keymap)
		simple-mpc-query-mode-map (make-sparse-keymap)
		simple-mpc-current-playlist-mode-map (make-sparse-keymap)
		simple-mpc-table-separator "="
		simple-mpc-playlist-format "%file%=%album% / %artist% ")

  (general-define-key
   :keymaps 'simple-mpc-mode-map
   "<return>" 'nil
   "lq" 'do-music-stop
   "ln" 'do-music-next
   "lh" 'do-music-previous
   "lp" 'do-music-toggle
   "ll" 'do-music-load-playlist
   "l/" 'do-music-query
   "lr" 'do-music-shuffle
   "lN" 'do-music-seek-forward
   "lH" 'do-music-seek-backward
   "ld" 'do-music-delete
   "lx" 'do-music-clear-playlist)

  (defun do--music-playlist-init ()
	(general-define-key
	 :keymaps 'local
	 :states '(normal visual)
	 "<return>" 'do-music-play-selected
	 "C-n" 'do-music-next
	 "C-h" 'do-music-previous))
  (add-hook 'simple-mpc-current-playlist-mode-hook 'do--music-playlist-init)

  (defun do--music-query-init ()
	(general-define-key
	 :keymaps 'local
	 :states '(normal visual)
	 "<return>" 'simple-mpc-query-add-and-play
	 "ls" 'do-music-query-sort
	 "lq" 'do-music-query-quit)

	(general-define-key
	 :keymaps 'local
	 "la" 'do-music-query-add))
  (add-hook 'simple-mpc-query-mode-hook 'do--music-query-init))

(provide 'do-music)
