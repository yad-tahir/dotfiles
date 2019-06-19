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

(evil-define-text-object do-evil-a-section
  (count &optional beg end select-type)
  "Select a function"
  :type exclusive
  :jump nil
  (save-excursion
	(evil-motion-loop (dir (or count 1))
	  (mark-defun dir))
	(setq beg (region-beginning)
		  end (region-end)))
  (evil-range beg end select-type :expanded t))

(evil-define-text-object do-evil-inner-section
  (count &optional beg end select-type)
  "Select inner function"
  :type exclusive
  :jump nil
  (evil-select-inner-object 'evil-defun beg end select-type count))


(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "b" 'do-evil-inner-section)

(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "b" 'do-evil-a-section)

;; There is no reason why paragraph text objects are selected as lines
(evil-define-text-object evil-a-paragraph (count &optional beg end type)
  "Select a paragraph."
  :type inclusive
  (evil-select-an-object 'evil-paragraph beg end type count))

(evil-define-text-object evil-inner-paragraph (count &optional beg end type)
  "Select inner paragraph."
  :type inclusive
  (evil-select-inner-object 'evil-paragraph beg end type count))
