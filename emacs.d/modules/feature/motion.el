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


;; Keymaps

(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "b" 'do-evil-inner-section
 "B" 'do-evil-whole-buffer
 "<SPC>" 'do-evil-whitespace)

(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "b" 'do-evil-a-section
 "B" 'do-evil-whole-buffer
 "<SPC>" 'do-evil-whitespace)

(general-define-key
 :keymaps 'motion
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
