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

;;;###autoload
(defun do-lemonbar-notify(&rest args)
  "Sends an event to the lemonbar"
  (ignore args)
  (ignore-errors
	(start-process-shell-command
	 "lemonbar-notifier"
	 'nil
	 (concat "echo 'b"
			 (format "%s" (length (do-list-buffers)))
			 "'> /tmp/lemon-panel-fifo-emacs"))))

;; Notify lemon-bar whenever we create a new buffer, delete one, switch to another.
(advice-add 'switch-to-buffer :after #'do-lemonbar-notify)
(advice-add 'find-file :after #'do-lemonbar-notify)
(advice-add 'evil-delete-buffer :after #'do-lemonbar-notify)
(with-eval-after-load 'ivy
  (advice-add 'ivy-switch-buffer-kill :after #'do-lemonbar-notify))

;;;###autoload
(defun do-list-buffers (&optional list)
  "Return list of user buffers. This function is used in i3Block to
display the number of open buffers in Emacs."

  (interactive)
  (when (null list)
	(setq list (buffer-list)))
  (let ((result '()))
	(while list
	  (let* ((buffer (car list))
			 (name (buffer-name buffer)))
		(and name
			 (not (string-equal name ""))
			 (/= (aref name 0) ?\s)
			 (push buffer result)))
	  (setq list (cdr list)))
	result))

