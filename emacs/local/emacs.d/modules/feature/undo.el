;;; package -- undo/redo module -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;; Redo/undo module

;;; Code:

(setq undo-limit 67108864 ; 64mb.
      undo-strong-limit 100663296 ; 96mb.
      undo-outer-limit 1006632960) ; 960mb.

(use-package undo-fu
  :ensure t
  :after evil
  :config
  (setq evil-undo-system 'undo-fu))

(provide 'do-undo)

;;; undo.el ends here
