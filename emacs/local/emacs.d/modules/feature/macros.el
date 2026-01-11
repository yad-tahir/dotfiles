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

(with-eval-after-load 'evil
  (require 'evil-common)
  (require 'evil-vars)
  (require 'evil-commands)
  (require 'edmacro)

  (defun do-start-macro (&optional reg)
	(interactive)
	(if defining-kbd-macro
		(message "Recording Macro already started")
	  (let ((reg (or reg
					 evil-this-register
					 ?q)))
		(evil-record-macro reg)
		(message "Recording a new macro [%c]" reg))))

  (defun do-stop-macro ()
	(interactive)
	(if defining-kbd-macro
		(progn
		  (evil-record-macro nil)
		  (message "Macro recorded"))
	  (message "No macro recording")))

(defun do-call-last-macro (&optional count reg)
  "Execute a macro with a COUNT prefix and optional REG."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
		 (or evil-this-register ?q)))

  (let* ((macro (evil-get-register reg)))
	(message "Executing register @%s, %d times" (char-to-string reg) count)
	(evil-execute-macro count macro)))

  (defun do-edit-register (&optional reg)
	"Opens a temporary buffer to edit the contents of a register.

By default, Reg @q is shown."
	(interactive)
	(let* ((reg (or reg
					evil-this-register
					?q))
		   (reg-text (evil-get-register reg t)))
	  (unless reg-text
		(message "Register @%c is empty." reg))
	  (with-current-buffer (get-buffer-create "*Edit Register*")
		(erase-buffer)
		(when reg-text
		  (insert (format-kbd-macro reg-text)))
		(switch-to-buffer (current-buffer)))))

  (defun do-save-register (&optional reg)
	"Saves the current buffer text back into register REG"
	(interactive)
	(let ((reg (or reg
				   evil-this-register
				   ?q))
		  ;; Parse the buffer text into actual macro key-codes
		  (macro-data (edmacro-parse-keys (buffer-string))))
	  (evil-set-register reg macro-data)
	  (kill-buffer (current-buffer))
	  (message "Buffer saved to Reg [%c]" reg)))

  (general-define-key
   :prefix "q"
   :states 'normal
   "" nil
   "[" 'do-start-macro
   "]" 'do-stop-macro
   "q" 'do-call-last-macro
   "e" 'do-edit-register
   "w" 'do-save-register)

  (general-define-key
   :prefix "q"
   :states 'visual
   "q" 'apply-macro-to-region-lines))

(provide 'do-macros)
