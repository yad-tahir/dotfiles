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

;; Remove compiler warnings
(eval-when-compile
  (declare-function calc-trail-display nil)
  (declare-function calc-alg-entry nil))

;;;###autoload
(defun do-calculator ()
  (interactive)
  (let ((calculator-frame-found nil))
	(dolist (m (frame-list))
	  (when (equal (frame-parameter m 'name) "calculator" )
		(with-current-buffer (window-buffer (frame-selected-window m))
		  (when (equal (format "%s" major-mode) "calc-mode")
			(setq calculator-frame-found t)
			(raise-frame m)))))
	(when (not calculator-frame-found)
	  (do-make-frame "calculator")
	  (full-calc)
	  (calc-trail-display 1)
	  (calc-alg-entry))))


(provide 'do-calculator)
