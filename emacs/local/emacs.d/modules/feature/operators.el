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

(evil-define-operator do-evil-indent (beginning end)
  "Indent current defun."
  :move-point nil
  (evil-indent beginning end))

(evil-define-operator do-evil-fixup-whitespace (beginning end)
  "Fixes white space in the surrounded area between BEGINNING and END."
  :move-point nil
  (save-excursion
	;; Create two markers to track beginning and end after deleting white spaces
	(let* ((e (copy-marker end))
		   (b (copy-marker beginning))
		   (avoid-regex "^\\|$\\|\\s\"")
		   (start-regex (concat avoid-regex "\\|\\s(")) ;; open delimiter
		   (end-regex (concat avoid-regex "\\|\\s)")) ;; close delimiter
		   )

	  ;; Fix Beginning
	  (goto-char b)
	  ;; If there is a white-space around BEGINNING
	  (when (or (looking-at "\\s-") ;; at beginning
				;; one char before it
				(save-excursion (forward-char 1)
								(looking-at "\\s-"))
				;; or even one char after it
				(save-excursion (forward-char -1)
								(looking-at "\\s-")))

		(delete-horizontal-space)

		(unless (or (looking-at start-regex)
					(save-excursion (forward-char -1)
									(looking-at start-regex)))
		  (insert ?\s)))

	  ;; Fix End
	  (goto-char e)
	  (when (or (looking-at "\\s-")
				(save-excursion (forward-char -1)
								(looking-at "\\s-"))
				(save-excursion (forward-char 1)
								(looking-at "\\s-")))

		(delete-horizontal-space)

		(unless (or (looking-at end-regex)
					(save-excursion (forward-char -1)
									(looking-at end-regex)))
		  (insert ?\s)))

	  ;; Assure GC
	  (set-marker b nil)
	  (set-marker e nil))))

  (evil-define-command do-evil-paste-previous-line
	(count &optional register yank-handler)
	(interactive "*P<x>")
	(let ((content (or (when register
						 (evil-get-register register))
					   (get-register (car (car register-alist))))))
	  (message "%s %s" content (string-suffix-p "\n" content))
	  (unless (and (stringp content)
				   (string-suffix-p "\n" content))
		(evil-insert-newline-above)))
	(evil-paste-before count register yank-handler))

  (evil-define-command do-evil-paste-next-line
	(count &optional register yank-handler)
	(interactive "*P<x>")
	(let ((content (or (when register
						 (evil-get-register register))
					   (get-register (car (car register-alist))))))
	  (message "%s %s" content (string-suffix-p "\n" content))
	  (unless (and (stringp content)
				   (string-suffix-p "\n" content))
		(evil-insert-newline-below)))
	(evil-paste-after count register yank-handler))

(evil-define-operator do-evil-paste (beg end type)
  "Deletes the target and pastes register 0 N times based on the prefix count."
  :move-point t
  :interactive "<R>" ;; <R> captures beg, end, type
  (let ((cnt (if current-prefix-arg
				 (prefix-numeric-value current-prefix-arg)
			   1)))
	(evil-delete beg end type ?_) ;; Paste it into 'black hole' so that the register " won't be effected
	(evil-paste-before cnt ?\")))

(provide 'do-operators)
