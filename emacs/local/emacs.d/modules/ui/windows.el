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

(defvar-local do--line-number-remap-cookie nil)
(defvar-local do--line-number-active-remap-cookie nil)

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
 "h" 'evil-window-decrease-width
 "n" 'evil-window-increase-width
 "C" 'evil-window-move-very-top
 "T" 'evil-window-move-very-bottom
 "H" 'evil-window-move-far-left
 "N" 'evil-window-move-far-right
 "=" 'evil-balance-window)

(general-define-key
 :keymaps 'override
 :prefix "SPC wf"
 :states '(normal visual)
 "" '(:ignore t :which-key "frame")
 "n" 'do-make-frame
 "q" 'evil-quit
 "f" 'other-frame)

;; (general-define-key
;;  :keymaps 'override
;;  "C-+" 'text-scale-increase
;;  "C-=" #'(lambda()(interactive)(text-scale-set 0))
;;  "C--" 'text-scale-decrease
;;  "<f2>" 'do-make-frame)

;; ;; non-evil buffers

(require 'face-remap)

(setq global-text-scale-adjust-resizes-frames t)

(defun do-zoom-in ()
  (interactive)
  (text-scale-increase 0.5))

(defun do-zoom-out ()
  (interactive)
  (text-scale-decrease 0.5))

(defun do-zoom-reset ()
  (interactive)
  (text-scale-set 0))

(general-define-key
 "C-+"  #'do-zoom-in
 "C-="  #'do-zoom-reset
 "C--"  #'do-zoom-out
 "<f2>" 'do-make-frame)

(defun do--fix-zoom-layout ()
  "Scale all line number faces and tighten spacing locally when zooming.

This function is needed to fix how text-scale operations interacts with
display line number."

  (if (< text-scale-mode-amount 0)
	  (setq-local line-spacing 0)
	(setq-local line-spacing nil))

  ;; Clear previous remappings to prevent "stacking"
  (when do--line-number-remap-cookie
	(face-remap-remove-relative do--line-number-remap-cookie))
  (when do--line-number-active-remap-cookie
	(face-remap-remove-relative do--line-number-active-remap-cookie))

  ;; Calc scale
  (let ((scale (expt text-scale-mode-step text-scale-mode-amount)))
	(setq do--line-number-remap-cookie
		  (face-remap-add-relative 'line-number :height scale))
	(setq do--line-number-active-remap-cookie
		  (face-remap-add-relative 'line-number-current-line :height scale))))

(add-hook 'text-scale-mode-hook #'do--fix-zoom-layout)

;;;###autoload
(defun do-make-frame (&optional fname display props)
  "Create a new frame and switch to it. This function can be called by Shells
  using `emacsclient` utility. The default `make-frame` function can be buggy on
  Emacs servers, especially on servers with cold start.

The first optional argument FNAME indicates the name of the new frame.

The second optional argument DISPLAY indicates the X display on which the new
frame should be shown. The default value is `:0`.

The third optional argument PROPS indicates the properties of the frame"

  (interactive)
  ;; Check arguments
  (when (null display)
	(setq display (getenv "DISPLAY")))
  ;; Construct the properties of the new frame
  (let* ((fp (list (cons 'name fname)
				  (cons 'display display)
				  (unless (boundp 'pgtk-initialized)
					'(window-system . x))
				  '(minibuffer . t)
				  props
				  ))
		(frame (make-frame fp)))
	;; Create the frame and switch to it
	(select-frame-set-input-focus frame)
	frame))

(setq-default frame-title-format
			  '(:eval (format "%s %s- %02d%%%% " (buffer-name)
							  (cond
							   (buffer-file-truename
								(concat "(" buffer-file-truename ")"))
							   (dired-directory
								(concat "(" dired-directory ")"))
							   (t ""))
							  (/ (line-number-at-pos)
								 0.01
								 (line-number-at-pos (point-max)))))
			  ;; Create new frames as much as possible. This works beautifully
			  ;; with tiling window managers such as i3 and BSPWM
			  pop-up-frames t
			  ;; Buffers that do not require pop-up frames
			  display-buffer-alist
			  '(("\\*Messages\\*" (display-buffer-in-side-window))))

(provide 'do-windows)
