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

;;; Code:
(use-package telephone-line
  :ensure t
  :demand t
  :init (declare-function telephone-line-mode nil)
  :config
  (set-face-attribute 'telephone-line-evil-normal nil
					  :background chocolate-theme-element
					  :foreground chocolate-theme-bg :weight 'normal)
  (set-face-attribute 'telephone-line-evil-insert nil
					  :background chocolate-theme-element+9
					  :foreground chocolate-theme-bg :weight 'normal)
  (set-face-attribute 'telephone-line-evil-motion nil
					  :background chocolate-theme-highlight
					  :foreground chocolate-theme-bg :weight 'normal)
  (set-face-attribute 'telephone-line-evil-visual nil
					  :background chocolate-theme-highlight+2
					  :foreground chocolate-theme-bg :weight 'normal)
  (set-face-attribute 'telephone-line-evil-operator nil
					  :background chocolate-theme-highlight
					  :foreground chocolate-theme-white+3 :weight 'normal)
  (set-face-attribute 'telephone-line-evil-emacs nil
					  :background chocolate-theme-element+4
					  :foreground chocolate-theme-bg :weight 'normal)
  (set-face-attribute 'telephone-line-accent-active nil
					  :background chocolate-theme-shadow+1
					  :foreground chocolate-theme-white+3 :weight 'normal)
  (set-face-attribute 'telephone-line-accent-inactive nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-shadow+3 :weight 'normal)
  (set-face-attribute 'telephone-line-error nil
					  :background 'nil
					  :foreground chocolate-theme-highlight :weight 'bold)
  (set-face-attribute 'telephone-line-unimportant nil
					  :background 'nil
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'telephone-line-warning nil
					  :background 'nil
					  :foreground chocolate-theme-element+10 :weight 'bold)

  (setq telephone-line-faces
		'((evil . telephone-line-modal-face)
		  (alert . (telephone-line-warning . telephone-line-warning))
		  (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
		  (nil . (mode-line . mode-line-inactive))))

  (defun do--status-bar-space-segment ()
	"A telephone line segment returns a single space."

	(lambda (face) (ignore face) " "))

  (defun do--status-bar-macro-segment ()
	"A telephone line segment for macro recording"

	(lambda (face)
	  (ignore face)
	  (if defining-kbd-macro
		  (if evil-this-macro
			  (telephone-line-raw
			   (format " REC:%s" (char-to-string evil-this-macro)) nil)
			(telephone-line-raw  " REC" nil))
		(telephone-line-raw nil nil))))
  (defun do--status-bar-register-segment ()
	"A telephone line segment for active register"

	(lambda (face)
	  (ignore face)
	  (if evil-this-register
		  (telephone-line-raw
		   (format " REG:%s" (char-to-string evil-this-register)) nil)
		(telephone-line-raw nil nil))))

  (defun do--status-bar-total-length-segment ()
	"A telephone line segment returns the total number of characters in the current
buffer."

	(lambda (face)
	  (ignore face)
	  (telephone-line-raw (format " %s " (point-max)) nil)))

  (defun do--status-bar-mode-line-segment ()
	"A telephone line segment returns the mode line of the current buffer."

	(lambda (face)
	  (ignore face)
	  (telephone-line-raw (concat
						   ;; Buffer name
						   (telephone-line-raw mode-line-buffer-identification t)
						   ;; Indicators for recursive edits
						   (let ((result " ")
								 (i (recursion-depth)))
							 (while (> i 0)
							   (setq i (- i 1))
							   (setq result (concat result "+")))
							 result)))))

  (defun do--status-bar-position-segment ()
	"Position segment imitating vim-airline's appearance."
	(lambda (face)
	  (ignore face)
	  (concat (format "%2d"
					  (/ (line-number-at-pos)
						 0.01 (line-number-at-pos (point-max))))
			  "%% "
			  ;; Row and column
			  "%3l:%2c")))

  (setq telephone-line-lhs '((alert . (do--status-bar-macro-segment
									   do--status-bar-register-segment))
							 (evil . (telephone-line-evil-tag-segment))
							 (accent . (telephone-line-vc-segment
										telephone-line-erc-modified-channels-segment
										telephone-line-process-segment))
							 (nil . (do--status-bar-mode-line-segment)))

		telephone-line-rhs '((nil . (telephone-line-flycheck-segment
									 telephone-line-misc-info-segment))
							 (accent . (telephone-line-major-mode-segment
										do--status-bar-space-segment
										telephone-line-filesize-segment ))
							 (evil . (do--status-bar-position-segment)))

		mode-line-modified ""
		telephone-line-primary-left-separator		'telephone-line-nil
		telephone-line-primary-right-separator		'telephone-line-nil
		telephone-line-secondary-left-separator     'telephone-line-nil
		telephone-line-secondary-right-separator	'telephone-line-nil
		telephone-line-evil-use-short-tag nil)

  (telephone-line-mode))

(use-package evil-anzu
  :ensure t
  :defer 10
  :after evil
  :config
  (set-face-attribute 'anzu-mode-line nil
					  :foreground chocolate-theme-element+4
					  :background chocolate-theme-bg )
  (set-face-attribute 'anzu-mode-line-no-match nil
					  :foreground chocolate-theme-highlight
					  :background chocolate-theme-bg ))

;; Make the color of the mode line dynamic
(defun do--status-bar-change-mode-line-color()
  (let ((color (cond (buffer-read-only
					  `(,chocolate-theme-bg . ,chocolate-theme-highlight+1))
					 ((minibufferp)
					  `(,chocolate-theme-bg . ,chocolate-theme-shadow+3))
					 ((buffer-modified-p)
					  `(,chocolate-theme-bg . ,chocolate-theme-highlight+2))
					 (t
					  `(,chocolate-theme-bg . ,chocolate-theme-white)))))
	(set-face-background 'mode-line (car color))
	(set-face-foreground 'mode-line (cdr color))))
(add-hook 'post-command-hook 'do--status-bar-change-mode-line-color)
(add-hook 'windmove-do-window-select 'do--status-bar-change-mode-line-color)
(add-hook 'find-file-hook 'do--status-bar-change-mode-line-color)


(provide 'do-status-bar)
