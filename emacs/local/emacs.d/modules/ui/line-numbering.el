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

(use-package display-line-numbers
  :demand t
  :config
  (setq display-line-numbers-current-absolute nil
		;; Narrowing does not make sense in visual type
		display-line-numbers-width 3
		;; Visual really useful for the evil mode
		display-line-numbers-type 'visual)

  ;; Theme
  (set-face-attribute 'line-number nil
					  :foreground chocolate-theme-shadow+3
					  :weight 'normal)
  (set-face-attribute 'line-number-current-line nil
					  :foreground chocolate-theme-bg
					  :background chocolate-theme-shadow+3
					  :weight 'normal)
  (set-face-attribute 'fringe nil
					  :background chocolate-theme-bg)
  ;; Start line numbering
  (global-display-line-numbers-mode))


(provide 'do-line-numbering)
