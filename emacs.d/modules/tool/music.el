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

(cl-eval-when (compile)
  (require 'simple-mpc))

(use-package simple-mpc
  :ensure t
  :commands (simple-mpc-next
			 simple-mpc-prev
			 simple-mpc-view-current-playlist)
  :preface
  (declare-function do-music-next nil)
  (declare-function do-music-previous nil)
  (declare-function do--music-playlist-init nil)
  (declare-function do--music-query-init nil)
  (declare-function do--music-setup-process nil)
  (defvar do--music-process-update nil)
  (defvar do--music-toggle-state nil)

  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC am" 'do-music-playlist)

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

	;; Setup external notifier
	(ignore-errors
	  (start-process-shell-command
	   "music-notify-external-process"
	   nil
	   "killall music.sh; ~/.config/lemon-bar/blocks/music.sh 1 > /tmp/lemon-panel-fifo"))
	nil)

  (defun do--music-refresh-buffer()
	;; This method can be called external by shell; check the init function in
	;; this file. Thus, we need to encapsulate runtime errors if any.
	(ignore-errors
	  (let ((b (get-buffer "*simple-mpc-current-playlist*"))
			(cb (window-buffer (selected-window))))
		;; Refresh when the selected window is the music playlist otherwise
		;; there is no need to refresh. Most likely, the buffer is invisible.
		(when (equal b cb)
		  ;; This temporary switch is needed because the current buffer can be a
		  ;; 'server' buffer.
		  (with-current-buffer b
			(call-interactively 'revert-buffer))))))

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
	  ;; Search for the window if it exists
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
				 "bspc node $(xdo id -a 'music-playlist') -f &> /dev/null"))))))
	  (when (not music-playlist-frame-found)
		(do-make-frame "music-playlist")
		(simple-mpc-view-current-playlist))))

  :config
  (setq simple-mpc-mode-map (make-sparse-keymap)
		simple-mpc-query-mode-map (make-sparse-keymap)
		simple-mpc-current-playlist-mode-map (make-sparse-keymap))

  (general-define-key
   :keymaps 'simple-mpc-mode-map
   :states '(normal visual)
   "<return>" 'nil
   "SPC lq" 'do-music-stop
   "SPC lp" 'simple-mpc-toggle
   "SPC l/" 'simple-mpc-query
   "SPC lr" 'simple-mpc-shuffle-current-playlist
   "SPC ln" 'do-music-next
   "SPC lh" 'do-music-previous
   "SPC lN" 'simple-mpc-seek-forward
   "SPC lH" 'simple-mpc-seek-backward
   "SPC ld" 'simple-mpc-clear-current-playlist)

  (defun do--music-playlist-init ()
	(general-define-key
	 :keymaps 'local
	 :states '(normal visual)
	 "<return>" 'do-music-play-selected
	 "C-n" 'do-music-next
	 "C-h" 'do-music-previous
	 "M-n" 'simple-mpc-seek-forward
	 "M-h" 'simple-mpc-seek-backward))
  (add-hook 'simple-mpc-current-playlist-mode-hook #'do--music-playlist-init)

  (defun do--music-query-init ()
	(general-define-key
	 :keymaps 'local
	 :states '(normal visual)
	 "<return>" 'simple-mpc-query-add-and-play
	 "a" 'simple-mpc-query-add
	 "SPC ls" 'simple-mpc-query-sort
	 [remap evil-quit] 'simple-mpc-query-quit))
  (add-hook 'simple-mpc-query-mode-hook #'do--music-query-init)

  ;; Start MPD
  (ignore-errors
	(let ((b (get-buffer "*Messages*")))
	  (shell-command "mpd" b b)
	  ;; Volume is primarily controlled by PulseAudio. We don't need another
	  ;; volume controller in Emacs
	  (simple-mpc-modify-volume-internal 100))))

(provide 'do-music)
