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
      delete-old-versions t
      make-backup-files nil
      auto-save-default nil
      delete-auto-save-files t
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      large-file-warning-threshold 10000000 ;; 10MB!

      ;; Mouse
      mode-line-default-help-echo nil ; disable mode-line mouseovers
      cursor-in-non-selected-windows nil  ; hide cursors in other windows
      mouse-yank-at-point nil  ; disable mouse craziness
      visible-cursor nil
      x-stretch-cursor t
      ;; Smooth scrolling please!
      ;; mouse-wheel-scroll-amount '(1 ((shift . 1)))
      mouse-wheel-progressive-speed nil ;; no scrolling acceleration
      mouse-wheel-follow-mouse t ;; scroll window under mouse

      scroll-step 1
      ;; scroll-conservatively 150
      ;; scroll-margin 10
      scroll-margin 150
      maximum-scroll-margin 0.5
      auto-window-vscroll nil

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
      confirm-kill-processes nil ;; Kill all processes without confirmation
      sentence-end-double-space nil
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
              tooltip-delay 5
              native-comp-async-report-warnings-errors nil
              warning-suppress-log-types '((with-editor))
              warning-suppress-types '((with-editor)))

(defalias 'yes-or-no-p 'y-or-n-p)


(setq default-frame-alist '((tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (font . "DejaVu Sans Mono-16")
                            (fullscreen . maximized)
                            (line-spacing . 0)
                            (font-backend . "xft")))

;; Keep base UI clean
(desktop-save-mode 0)
(blink-cursor-mode 1) ;; blank during is controlled by `blink-cursor-blinks'
(scroll-bar-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode nil) ;; Redirect tool-tip messages to the echo area
(global-auto-revert-mode +1) ;; Refresh unmodified buffers if the backed files have changed
(global-visual-line-mode 1)
(context-menu-mode -1)

(provide 'do-general)
