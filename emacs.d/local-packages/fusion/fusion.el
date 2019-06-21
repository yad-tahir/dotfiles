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
;;; Fusion - Add evil operators to split and join text lines.
;;;
;;; Author: Dr. Yad Tahir <yad (at) ieee.org>
;;; Keywords: join text lines, wrap lines
;;;
;;; License:
;;; Copyright (C) 2019
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

(defcustom fusion-split-column fill-column
  "Column beyond which fusion automatically splits line."
  :group 'fusion
  :type 'number)

(defcustom fusion-indent-aware t
  "If enabled (t), keep text indentation after join and split operations."
  :group 'fusion
  :type 'boolean)

(defun fusion--fill-region (beginning end column)
  (unless beginning
	(setq beginning (region-beginning)))
  (unless end
	(setq end (region-end)))
  (setq end (- end 1))
  (when (< beginning end)
	(let ((fill-column column))
	  (if fusion-indent-aware
		  (progn
			(back-to-indentation)
			(let ((left-margin (current-column)))
			  (fill-region-as-paragraph beginning end)))
		(fill-region-as-paragraph beginning end)))))

;;;###autoload
(evil-define-operator fusion-join (beginning end)
  "Replaces new line chars in region by single spaces while keeping the
  structure of the paragraphes as much as possible.

The second argument BEGINNING indicates the starting position of the region.
Passing nil makes the region starts from (region-beginning).

The third argument END indicates the starting position of the region. Passing
nil makes the region starts from (region-end).

	This evil operator is the reverse of 'do-wrap-region'"

  ;; Joining a region is just a fill region operation with a large fill column
  (fusion--fill-region beginning end most-positive-fixnum))

;;;###autoload
(evil-define-operator fusion-split (beginning end)
  "Fills the selected region and makes it indent.

The second argument BEGINNING indicates the starting position of the region.
Passing nil makes the region starts from (region-beginning).

The third argument END indicates the starting position of the region. Passing
nil makes the region starts from (region-end)."

  (fusion--fill-region beginning end fusion-split-column))

(provide 'fusion)

;;; fusion.el ends here
