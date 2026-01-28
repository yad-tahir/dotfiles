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

(use-package define-word
  :ensure t
  :commands (define-word define-word-at-point)
  :init
  (evil-define-operator do-define-word (beg end)
    :motion evil-inner-word
    (define-word (buffer-substring-no-properties beg end) nil))

  (general-define-key
   :states 'normal
   "SPC lI" 'do-define-word))

(provide 'do-english)
