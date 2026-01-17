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

(eval-after-load 'man
  (general-define-key
   ;; Don't but buffer modifier in the override keymap, e.g. evil-change
   ;; In some modes, you need to override them, e.g. magit
   :keymaps 'Man-mode-map
   :states '(normal visual)
   "C-n" 'Man-next-manpage
   "C-h" 'Man-previous-manpage
   "C-c" 'Man-previous-section
   "C-t" 'Man-next-section))

(defun do-man (cmd)
  (interactive)
  (let ((frame nil))
    (unless (display-graphic-p (selected-frame))
      ;; Force emacs to have a frame; Otherwise	`man' will crash in emacs server!
      (setq frame (do-make-frame)))
    (require 'man)
    (man (concat "-a " cmd))
    ;; Since `man' has already created a new frame, close the first one
    (when frame
      (delete-frame frame))))

(provide 'do-man)
