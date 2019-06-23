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


(cl-eval-when (compile)
  (require 'evil))

;; Operators
(evil-define-operator do-evil-insert (beginning end &optional type)
  "Ask for a motion and switch to the insert state at BEGINNING."
  (ignore end beginning)

  (cond ((string= type "line")
		 (if (evil-visual-state-p)
			 (call-interactively 'evil-insert)
		   (call-interactively 'evil-insert-line)))
		(t
		 (call-interactively 'evil-insert))))

(evil-define-operator do-evil-append (beginning end &optional type)
  "Ask for a motion and switch to the insert state at END."
  ;; Go to the beginning of the region to repeat the operation correctly in
  ;; Visual Line and Visual Block.
  (cond ((string= type "line")
		 (goto-char beginning)
		 (evil-end-of-line))
		((string= type "block")
		 (goto-char beginning))
		((string= type "inclusive")
		 (if (evil-visual-state-p)
			 (goto-char end)
		   ;; We shift one character because evil-append will add one
		   (goto-char (- end 1))))
		(t
		 (goto-char end)))
  (call-interactively 'evil-append))

(evil-define-operator do-evil-forward-motion (beginning end)
  "Ask for a motion and move forward."
  (ignore beginning)
  (goto-char end))


(evil-define-operator do-evil-backward-motion (beginning end)
  "Ask for a motion and move backward."
  (ignore end)
  (goto-char beginning))

(evil-define-operator do-evil-narrow (beginning end)
  "Ask for a motion and move backward."
  (narrow-to-region beginning end))
;; Adjust some properties to make them more useful with the 'delete' and
;; 'change' operators
(evil-put-command-property 'evil-previous-line-first-non-blank :type 'exclusive)
(evil-put-command-property 'evil-next-line-first-non-blank :type 'exclusive)
