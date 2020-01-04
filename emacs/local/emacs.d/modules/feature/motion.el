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


;; Custom motions
(evil-define-motion do-evil-forward-whitespace (count)
  "Moves point COUNT whitespaces forward."
  :jump nil
  (evil-signal-at-bob-or-eob count)
  (evil-forward-beginning 'whitespace (or count 1))
  (beginning-of-thing 'whitespace))

(evil-define-motion do-evil-backward-whitespace (count)
  "Moves point COUNT whitespaces backward."
  :jump nil
  (evil-signal-at-bob-or-eob count)
  (evil-backward-beginning 'whitespace (or count 1))
  (beginning-of-thing 'whitespace))

(evil-define-motion do-evil-forward-comment (count)
  "Move the cursor to the beginning of the COUNT-th next comment block."
  :jump t
  (evil-motion-loop (v (or count 1))
	;; Exist from the current comment block
	(while (evil-in-comment-p)
	  (when (evil-looking-at-start-comment)
		(forward-char))
	  (evil-forward-word-begin 1)
	  (goto-char (cdr (bounds-of-evil-comment-at-point))))

	;; Search for the next comment block
	(while (not (evil-in-comment-p))
	  (evil-forward-word-begin 1)
	  (forward-char 1)))
  (goto-char (evil-in-comment-p)))

(evil-define-motion do-evil-backward-comment (count)
  "Move the cursor to the beginning of the COUNT-th previous comment block."
  :jump t
  (evil-motion-loop (v (or count 1))
	;; Exist from the current comment block
	(while (evil-in-comment-p)
	  (goto-char (evil-in-comment-p))
	  (evil-backward-word-begin 1))

	;; Search for the next comment block going backward
	(while (not (evil-in-comment-p))
	  (evil-backward-word-begin 1))

	;; Go to the beginning of this block
	(while (evil-in-comment-p)
	  (goto-char (evil-in-comment-p))
	  (evil-backward-word-begin 1)))
  (evil-forward-word-begin 1))

;; Adjust some properties to make them more useful with the 'delete' and
;; 'change' operators
(evil-put-command-property 'evil-previous-line-first-non-blank :type 'exclusive)
(evil-put-command-property 'evil-next-line-first-non-blank :type 'exclusive)

;; The default 'evil-line' motion assumes that line navigation is done by
;; calling 'evil-next-line' and 'evil-previous-line'. However, this is not the
;; case in my settings as I use visual-line operations instead. Sadly, this
;; makes the implementation buggy. The code below is simple workaround to
;; address this issue.
(evil-define-motion evil-line (count)
  "Move COUNT - 1 lines down."
  :type line
  (let (line-move-visual)
	(condition-case err
		;; @HACK: provide 't' as the second argument to ignore errors
		(evil-line-move (1- (or count 1)) t)
	  ((beginning-of-buffer end-of-buffer)))))


;; Custom text objects

(evil-define-text-object do-evil-a-section (count &optional beg end type)
  "Select a function."
  :jump nil
  (save-excursion
	(evil-motion-loop (dir (or count 1))
	  (mark-defun dir))
	(setq beg (region-beginning)
		  end (region-end)))
  (evil-range beg end type :expanded t))

(evil-define-text-object do-evil-inner-section (count &optional beg end type)
  "Select inner function."
  :jump nil
  (evil-select-inner-object 'evil-defun beg end type (or count 1)))

(evil-define-text-object do-evil-whole-buffer (count &optional beg end type)
  :type line
  "Select the whole buffer."
  (evil-range (point-min) (point-max) type))

;; There is no reason why paragraph text objects are selected as lines
(evil-define-text-object evil-a-paragraph (count &optional beg end type)
  "Select a paragraph."
  :type inclusive
  (evil-select-an-object 'evil-paragraph beg end type count))

(evil-define-text-object evil-inner-paragraph (count &optional beg end type)
  "Select inner paragraph."
  :type inclusive
  (evil-select-inner-object 'evil-paragraph beg end type count))

(evil-define-text-object do-evil-a-whitespace (count &optional beg end type)
  "Select a whitespace."
  :type inclusive
  (ignore beg end count)
  (let ((bounds (bounds-of-thing-at-point 'whitespace)))
	;; When there is no whites pace
	(unless bounds
	  (error "No whitespace found"))
	(evil-range (car bounds) (cdr bounds) type :expanded t)))

(evil-define-text-object do-evil-inner-comment (count &optional beg end type)
  "Select inner comment block."
  :type exclusive
  :move-point nil

  (save-excursion
	(unless (evil-in-comment-p)
	  ;; Skip indentation and retry
	  (evil-last-non-blank)
	  (unless (evil-in-comment-p)
		(error "No comment section found")))

	(setq beg (evil-in-comment-p)
		  end (+ 1 beg)
		  count (or count 1))

	(let ((b beg)
		  (e end))
	  ;; Find the start of the comment section
	  (ignore-errors
		(while b
		  (evil-backward-word-end 1)
		  (setq beg b
				b (evil-in-comment-p))))

	  ;; Find the end of the comment section
	  (evil-motion-loop (k count)
		(ignore-errors
		  (goto-char e)
		  ;; Add one, needed by `bounds-of-evil-comment-at-point'
		  (when (evil-looking-at-start-comment)
			(evil-forward-char 1))
		  (while e
			(forward-char)
			(evil-forward-word-end 1)
			(setq end e
				  e (cdr (bounds-of-evil-comment-at-point)))))
		(setq e (+ 1 end)))))

  (unless (eq end (point-max))
	(setq end (- end 1))) ;; Exclude last \n
  (evil-range beg end type :expanded t))

(evil-define-text-object do-evil-a-comment (count &optional beg end type)
  "Select a comment block."
  :type exclusive
  :move-point nil

  (let ((range (do-evil-inner-comment count beg end type)))
	(setq beg (save-excursion
				;; Include prefix whitespace
				(goto-char (car range))
				(ignore-errors (do-evil-backward-whitespace))
				(point))
		  end (save-excursion
				;; Include last \n
				(goto-char (nth 1 range))
				(ignore-errors (evil-forward-char 1 t))
				(point)))
	(evil-range beg end type :expanded t)))

;; Keymaps

(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "b" 'do-evil-inner-section
 "B" 'do-evil-whole-buffer
 "c" 'do-evil-inner-comment
 "<SPC>" 'do-evil-whitespace)

(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "b" 'do-evil-a-section
 "c" 'do-evil-a-comment
 "B" 'do-evil-whole-buffer
 "<SPC>" 'do-evil-whitespace)

(general-define-key
 :keymaps 'motion
 "] c" 'do-evil-forward-comment
 "[ c" 'do-evil-backward-comment
 "] <SPC>" 'do-evil-forward-whitespace
 "[ <SPC>" 'do-evil-backward-whitespace)


;; Third-party packages

(use-package evil-matchit
  :ensure t
  :commands (evilmi-jump-items)
  :init
  (general-define-key
   :states '(normal visual)
   "%" 'evilmi-jump-items)
  :config
  (global-evil-matchit-mode 1))

(use-package avy
  :ensure t
  :commands (avy-goto-char-2 avy-resume)
  :init
  (general-define-key
   :states '(normal visual)
   "g/" 'avy-goto-char-2
   "g?" 'avy-resume)

  :config
  (set-face-attribute 'avy-lead-face nil
					  :background nil
					  :foreground nil
					  :inherit 'diff-refine-removed)
  (set-face-attribute 'avy-lead-face-0 nil
					  :background nil
					  :foreground nil
					  :inherit 'diff-refine-added)
  (set-face-attribute 'avy-lead-face-1 nil
					  :background nil
					  :foreground nil
					  :inherit 'diff-refine-changed)
  (set-face-attribute 'avy-lead-face-2 nil
					  :background nil
					  :foreground nil
					  :inherit 'diff-nonexistent))


(provide 'do-mation)
