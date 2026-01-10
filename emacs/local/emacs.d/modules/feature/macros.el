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

(general-define-key
 :prefix "q"
 :states '(normal visual)
 "" nil
 "[" 'start-kbd-macro
 "]" 'end-kbd-macro
 "@" 'evil-record-macro
 "u" 'kmacro-edit-macro
 "q" 'call-last-kbd-macro
 "n" 'name-last-kbd-macro
 "i" 'insert-kbd-macro
 ";" 'counsel-M-x)

(general-define-key
 :prefix "q"
 :states 'visual
 "q" 'apply-macro-to-region-lines)

(general-define-key
 :keymaps 'edmacro-mode-map
 "SPC lw" 'edmacro-finish-edit
 "SPC lq" 'kill-buffer)


(provide 'do-macros)
