;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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
(defun do-polybar-notify(&rest args)
  "Sends an event to the lemonbar"
  (ignore args)
  (ignore-errors
	(start-process-shell-command
	 "polybar-notifier"
	 'nil
	 "polybar-msg hook emacs 1")))

;; Notify polybar whenever we create a new buffer, delete one, switch to another.
(advice-add 'switch-to-buffer :after #'do-polybar-notify)
(advice-add 'find-file :after #'do-polybar-notify)
(advice-add 'evil-delete-buffer :after #'do-polybar-notify)
(with-eval-after-load 'ivy
  (advice-add 'ivy-switch-buffer-kill :after #'do-polybar-notify))

(provide 'do-polybar-bar)
