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

;; Windows and frames configurations
(general-define-key
 :keymaps 'override
 :prefix "SPC w"
 :states '(normal visual)
 ;; Make a prefix-command and add description
 "" '(:ignore t :which-key "window")
 "w" 'evil-window-next
 "W" 'evil-window-prev
 "-" '(lambda()(interactive)(split-window-vertically)(evil-window-next 0))
 "/" '(lambda()(interactive)(split-window-horizontally)(evil-window-next 0))
 "q" 'evil-window-delete
 "r" 'evil-window-rotate-downwards
 "c" 'evil-window-increase-height
 "t" 'evil-window-decrease-height
 "h" 'evil-window-increase-width
 "n" 'evil-window-decrease-width
 "C" 'evil-window-move-very-top
 "T" 'evil-window-move-very-bottom
 "H" 'evil-window-move-far-left
 "N" 'evil-window-move-far-right
 "=" 'evil-balance-window)

(general-define-key
 :keymaps 'override
 :prefix "SPC f"
 :states '(normal visual)
 "" '(:ignore t :which-key "frame")
 "n" 'do-new-frame
 "q" 'evil-quit
 "f" 'other-frame)

(general-define-key
 "C-+" 'text-scale-increase
 "C-=" '(lambda()(interactive)(text-scale-set 0))
 "C--" 'text-scale-decrease)

;;;###autoload
(defun do-make-frame (&optional fname display props)
  "Create a new frame and switch to it. This function can be called by Shells
  using 'emacsclient' utility. The default 'make-frame' function can be buggy on
  Emacs servers, especially on servers with cold start.

The first optional argument FNAME indicates the name of the new frame.

The second optional argument DISPLAY indicates the X display on which the new
frame should be shown. The default value is ':0'.

The third optional argument PROPS indicates the properties of the frame"

  (interactive)
  ;; Check arguments
  (when (null display)
	(setq display ":0"))
  ;; Construct the properties of the new frame
  (let ((fp (or props (list (cons 'name fname)
							(cons 'display display)
							'(window-system . x)
							'(minibuffer . t)
							props
							))))
	;; Create the frame and switch to it
	(select-frame-set-input-focus (make-frame fp))))

(setq-default frame-title-format
			  '(:eval (format "%s %s - %3d%%%%" (buffer-name)
							  (cond
							   (buffer-file-truename
								(concat "(" buffer-file-truename ")"))
							   (dired-directory
								(concat "(" dired-directory ")"))
							   (t ""))
							  (/ (line-number-at-pos)
								 0.01
								 (line-number-at-pos (point-max)))))
			  ;; Use new frames as much as possible
			  pop-up-frames t)

(provide 'do-windows)
;;; windows.el ends here
