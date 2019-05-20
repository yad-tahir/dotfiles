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

(use-package simple-mpc
  :ensure t
  :commands (simple-mpc-next
			 simple-mpc-prev
			 do-music-toggle
			 simple-mpc-view-current-playlist
			 simple-mpc-query)
  :preface
  (declare-function do-music-next nil)
  (declare-function do-music-previous nil)
  (declare-function do--music-playlist-init nil)
  (declare-function do--music-query-init nil)
  :init

  (setq do--music-process-update nil
		do--music-toggle-state nil)

  (general-define-key
   :prefix "SPC m"
   :keymaps 'override
   :states '(normal visual)
   "" '(:ignore t :which-key "music player")
   "n" 'do-music-next
   "h" 'do-music-previous
   "m" 'do-music-toggle
   "s" 'simple-mpc-query
   "l" 'do-music-playlist)

  (defun do--music-setup-process ()
	(ignore-errors
	  (kill-process do--music-process-update))
	(when do--music-toggle-state
	  (ignore-errors
		(setq do--music-process-update
			  (start-process-shell-command
			   "music-notify-process"
			   nil
			   "while [ 1 ]; do emacsclient -nqe '(do--music-refresh-buffer)' && mpc current --wait; done"))))
	(do--music-notify-external)
	nil)

  (defun do--music-refresh-buffer()
	;; This method can be called external by shell; check the init function in this file.
	;; Thus, we need to encapsulate runtime errors if any.
	(ignore-errors
	  (let ((b (get-buffer "*simple-mpc-current-playlist*"))
			(cb (window-buffer (selected-window))))
		;; To avoid i3 notifications, we refresh when the selected window is the music playlist
		;; otherwise there is no need to refresh. Most likely, the buffer is invisible.
		;; (when (equal b cb)
		;; This temporary switch is needed because the current buffer can be 'server buffer'.
		;; This usually occurs when you have a fresh emacsclient that has not selected any buffer yet.
		;; For instance, emacsclient -cqe '(do--music-refresh-buffer)'.
		(with-current-buffer b
		  (call-interactively 'revert-buffer)))))

  (defun do-music-next ()
	(interactive)
	(setq do--music-toggle-state t)
	(do--music-setup-process)
	(simple-mpc-next))

  (defun do-music-previous ()
	(interactive)
	(setq do--music-toggle-state t)
	(do--music-setup-process)
	(simple-mpc-prev))

  (defun do-music-toggle ()
	(interactive)
	(require 'simple-mpc)
	;; (simple-mpc-toggle)
	(if do--music-toggle-state
		(progn
		  (start-process-shell-command "mpc" 'nil "/usr/bin/mpc pause")
		  (setq do--music-toggle-state nil))
	  (progn
		(start-process-shell-command "mpc" 'nil "/usr/bin/mpc play")
		(setq do--music-toggle-state t)))
	(do--music-setup-process))

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

;;;###autoload
  (defun do-music-playlist ()
	(interactive)
	(let ((music-playlist-frame-found nil))
	  (dolist (m (frame-list))
		(when (equal (frame-parameter m 'name) "music-playlist" )
		  (with-current-buffer (window-buffer (frame-selected-window m))
			(when (equal (format "%s" major-mode) "simple-mpc-mode")
			  (setq music-playlist-frame-found t)
			  (raise-frame m)
			  (ignore-errors
				(start-process-shell-command
				 "notify-bspc-process"
				 nil
				 "bspc node $(xdo id -a 'music-playlist') -f &> /dev/null"))

			  ))))
	  (when (not music-playlist-frame-found)
		(do-make-frame "music-playlist")
		(simple-mpc-view-current-playlist))))

  :config
  (setq simple-mpc-mode-map (make-sparse-keymap)
		simple-mpc-query-mode-map (make-sparse-keymap)
		simple-mpc-current-playlist-mode-map (make-sparse-keymap))


  (general-define-key
   :prefix "SPC m"
   :keymaps 'override
   :states '(normal visual)
   "" '(:ignore t :which-key "music player")
   "q" 'do-music-stop
   "r" 'simple-mpc-shuffle-current-playlist
   "p" 'simple-mpc-toggle
   "x" 'simple-mpc-clear-current-playlist)

  (general-define-key
   :keymaps 'simple-mpc-mode-map
   :states 'normal
   "<return>" 'nil
   "s" #'simple-mpc-query
   "w" #'simple-mpc-seek-forward
   "b" #'simple-mpc-seek-backward)

  (defun do--music-playlist-init ()
	(general-define-key
	 :keymaps 'local
	 :states '(normal visual)
	 "<return>" 'do-music-play-selected
	 "r" #'simple-mpc-shuffle-current-playlist
	 "N" #'do-music-next
	 "H" #'do-music-previous
	 "x" #'simple-mpc-clear-current-playlist
	 "C" #'simple-mpc-clear-current-playlist
	 "d" #'simple-mpc-delete))
  (add-hook 'simple-mpc-current-playlist-mode-hook #'do--music-playlist-init)

  (defun do--music-notify-external ()
	(ignore-errors
	  (start-process-shell-command
	   "music-notify-external-process"
	   nil
	   "killall music.sh; ~/.config/lemon-bar/blocks/music.sh 1 > /tmp/lemon-panel-fifo")))

  (defun do--music-query-init ()
	(general-define-key
	 :keymaps 'local
	 :states '(normal visual)
	 "a" 'simple-mpc-query-add
	 "i" 'simple-mpc-query-add
	 "<return>" 'simple-mpc-query-add-and-play))
  (add-hook 'simple-mpc-query-mode-hook #'do--music-query-init)

  (ignore-errors
	;; start mpd and mpc

	(shell-command "mpd" (get-buffer "*Messages*") (get-buffer "*Messages*"))

	;; Volume will be controlled by pulse.
	;; We don't need another volume controller for mpc.
	(simple-mpc-modify-volume-internal 100)))
