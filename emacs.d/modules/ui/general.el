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

(setq inhibit-startup-message t
	  inhibit-startup-echo-area-message t
	  user-mail-address "yad@ieee.org"

	  ;; Windows/Frames
	  frame-inhibit-implied-resize t
	  highlight-nonselected-windows nil
	  use-dialog-box nil ;; avoid GUI
	  auto-window-vscroll nil
	  max-mini-window-height 0.35
	  resize-mini-windows t

	  ;; Files
	  delete-old-versions -1
	  make-backup-files t
	  backup-directory-alist `((".*" . ,temporary-file-directory))
	  auto-save-default t
	  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	  coding-system-for-read 'utf-8
	  coding-system-for-write 'utf-8
	  large-file-warning-threshold 100000000 ;; 100MB!

	  ;; Mouse
	  mode-line-default-help-echo nil ; disable mode-line mouseovers
	  cursor-in-non-selected-windows nil  ; hide cursors in other windows
	  mouse-yank-at-point nil  ; disable mouse craziness
	  visible-cursor nil
	  x-stretch-cursor nil
	  ;; Smooth scrolling please!
	  ;; mouse-wheel-scroll-amount '(1 ((shift . 1)))
	  mouse-wheel-progressive-speed nil ;; no scrolling acceleration
	  mouse-wheel-follow-mouse t ;; scroll window under mouse

	  ;; Keyword
	  scroll-step 1
	  scroll-conservatively 10000
	  scroll-margin 100
	  maximum-scroll-margin 0.5

	  ;; Performance
	  ;; Defer jit font locking slightly to [try to] improve Emacs performance
	  jit-lock-defer-time nil
	  jit-lock-stealth-nice 0.1
	  jit-lock-stealth-time 0.2
	  jit-lock-stealth-verbose nil
	  max-lisp-eval-depth 10000
	  bidi-display-reordering nil ; tiny performance boost
	  gc-cons-threshold 20000000
	  uniquify-buffer-name-style 'forward
	  uniquify-strip-common-suffix nil
	  inhibit-compacting-font-caches t

	  ;; Other
	  blink-matching-paren nil ;; don't blink--too distracting
	  confirm-kill-processes nil ;; Kill all pprocesses without confirmation
	  sentence-end-double-space nil
	  ;; debug-on-error t
	  ;; image-animate-loop t
	  indicate-buffer-boundaries nil
	  indicate-empty-lines nil
	  show-help-function nil          ; hide :help-echo text
	  ;; Disable multi-line echo messages. They are really annoying when you
	  ;; jump between lines.
	  eldoc-echo-area-use-multiline-p nil
	  ;; No beeping or blinking
	  ring-bell-function 'ignore
	  visible-bell nil)

(setq-default fill-column 80
			  tab-width 4
			  tooltip-delay 5
			  indent-tabs-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)

(desktop-save-mode 0)
(tooltip-mode nil) ;; Redirect tooltip messages to the echo area

;; Disabled by .Xresources
;; (scroll-bar-mode 0)
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)


(provide 'do-general)
