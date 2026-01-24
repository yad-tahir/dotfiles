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

(use-package display-line-numbers
  :after evil
  :config
  (setq display-line-numbers-current-absolute t
        display-line-numbers-width 4
        display-line-numbers-grow-only t
        ;; Narrowing does not make sense in visual type
        display-line-numbers-type 'visual)
  ;; Start line numbering
  (global-display-line-numbers-mode 1))


(defun do-line-numbers-to-visual ()
  "Change the mode of line numbers for the current buffer to visual mode"
  (interactive)
  (setq-local display-line-numbers-type 'visual
              visual-line-mode t)
  (display-line-numbers-mode 1))

(defun do-line-numbers-to-relative ()
  "Change the mode of line numbers for the current buffer to relative mode"
  (interactive)
  (setq-local display-line-numbers-type 'relative)
  (when display-line-numbers-mode
    (display-line-numbers-mode 0))
  (display-line-numbers-mode 1))

(provide 'do-line-numbering)
