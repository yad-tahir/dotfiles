;;; -*- ;lexical-binding: t; -*-
;;;
;;;      ______           _
;;;     / ____/_  _______(_)___  ____
;;;    / /_  / / / / ___/ / __ \/ __ \
;;;   / __/ / /_/ (__  ) / /_/ / / / /
;;;  /_/    \__,_/____/_/\____/_/ /_/
;;;
;;;
;;; package -- Summary
;;; Fusion - Provides evil operators to split and join text lines.
;;;
;;; Author: Dr. Yad Tahir <yad (at) ieee.org>
;;; Keywords: join text lines, wrap lines
;;;
;;; License:
;;; Copyright (C) 2026
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301, USA.
;;;

(require 'evil)
(provide 'evil-macros)

(defcustom fusion-fill-column fill-column
  "Column beyond which Fusion splits line."
  :group 'fusion
  :type 'number)

(defcustom fusion-indent-aware t
  "If enabled (t), keep text indentation after applying join and split
  operations."
  :group 'fusion
  :type 'boolean)

;;;###autoload
(evil-define-operator fusion-join (beginning end)
  "Joins the lines defined between BEGINNING and END.

This evil operator is the reverse of `fusion-split'."

  :move-point nil ;; don't move the cursor position to BEGINNING
  :type line
  (evil-join beginning end))

;;;###autoload
(evil-define-operator fusion-split (beginning end)
  "Fills the region defined between BEGINNING and END, and indents it as
necessary.

This evil operator is the reverse of `fusion-join'."

  :move-point nil ;; don't move the cursor position to BEGINNING
  :type line
  (let ((fill-column fusion-fill-column))
	(save-excursion
	  (if fusion-indent-aware
		  (progn
			(back-to-indentation)
			(let ((left-margin (current-column)))
			  (fill-region beginning end)))
		(fill-region beginning end)))))

(provide 'fusion)

;;; fusion.el ends here
