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
					  :background chocolate-theme-element+4
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
					  :background chocolate-theme-element+8
					  :foreground chocolate-theme-bg :weight 'normal)
  (set-face-attribute 'telephone-line-accent-active nil
					  :background chocolate-theme-shadow+1
  					  :foreground chocolate-theme-white+3 :weight 'normal)
  (set-face-attribute 'telephone-line-accent-inactive nil
					  :background chocolate-theme-bg
					  :foreground chocolate-theme-shadow+3 :weight 'normal)
  (set-face-attribute 'telephone-line-error nil :background 'nil
					  :foreground "orangered1" :weight 'bold)

  (defun do--status-bar-space-segment ()
	"A telephone line segment returns a single space."

	(lambda (face) (ignore face) " "))

  (defun do--status-bar-total-length-segment ()
	"A telephone line segment returns the total number of characters in the current
buffer."

	(lambda (face)
	  (ignore face) 
	  (telephone-line-raw (format " %s " (point-max)) nil) ) )

  (defun do--status-bar-mode-line-segment ()
	"A telephone line segment returns the mode line of the current buffer."

	(lambda (face)
	  (ignore face) 
	  (telephone-line-raw mode-line-buffer-identification t)))

(defun do--status-bar-position-segment ()
  "Position segment imitating vim-airline's appearance."
  (lambda (face)
	(ignore face) 
	(concat (format "%2d%%"
					(/ (line-number-at-pos) 0.01 (line-number-at-pos (point-max))))
			"% %3l:%2c")))

  (setq telephone-line-lhs '((evil   . (telephone-line-evil-tag-segment))
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
		telephone-line-secondary-left-separator	'telephone-line-nil
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

;;; status-bar.el ends here
