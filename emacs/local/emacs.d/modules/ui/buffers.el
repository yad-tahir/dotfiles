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

;; Add key bindings to manage buffers in the evil mode
(general-define-key
 :keymaps 'override
 :prefix "SPC b"
 :states '(normal visual)
 "" '(:ignore t :which-key "buffer")
 "n" 'evil-buffer-new
 "q" 'evil-delete-buffer
 "d" 'evil-delete-buffer
 "b" 'evil-prev-buffer
 "B" 'evil-next-buffer
 "l" 'ibuffer
 "s" 'evil-switch-to-windows-last-buffer
 "c" 'clone-indirect-buffer-other-window)


(general-define-key
 :prefix "SPC s"
 :states 'normal
 "b" 'switch-to-buffer)

(general-define-key
 "<f5>" 'revert-buffer
 "<f3>" '(lambda() (interactive)
           (evil-delete-buffer (current-buffer))))

;; Remove the need for a confirmation when process buffer is killed
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;;###autoload
(defun do-list-buffers (&optional list)
  "Returns list of user buffers. This function is used in i3Block to
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

(defun do--display-warnings (type message &optional level buffer-name)
  "Display WARNING in the minibuffer instead of creating a new buffer."
  (unless (equal level :debug)
    (message "*EXCEPTION* (%s)(%s): %s %s" level buffer-name type message )))

(advice-add 'display-warning :override #'do--display-warnings)

(use-package all-the-icons
  :ensure t
  :disabled t
  :if (display-graphic-p))

(provide 'do-buffers)
