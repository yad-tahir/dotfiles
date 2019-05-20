;;; -*- lexical-binding: t; -*-

(setq inhibit-startup-message t
	  inhibit-startup-echo-area-message t
	  user-mail-address "me@yad.email"
	  ;; debug-on-error t
	  bidi-display-reordering nil ; tiny performance boost
	  blink-matching-paren nil    ; don't blink--too distracting
	  cursor-in-non-selected-windows nil  ; hide cursors in other windows
	  frame-inhibit-implied-resize t
	  confirm-kill-processes nil ;; Kill all pprocesses without confirmation
	  highlight-nonselected-windows nil
	  delete-old-versions -1
	  make-backup-files t
	  backup-directory-alist `((".*" . ,temporary-file-directory))
	  auto-save-default t
	  auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
	  coding-system-for-read 'utf-8
	  coding-system-for-write 'utf-8
	  sentence-end-double-space nil
	  max-lisp-eval-depth 10000
	  gc-cons-threshold 20000000
	  large-file-warning-threshold 100000000 ;; 100MB!
	  ;; image-animate-loop t
	  indicate-buffer-boundaries nil
	  indicate-empty-lines nil
	  max-mini-window-height 0.3
	  mode-line-default-help-echo nil ; disable mode-line mouseovers
	  mouse-yank-at-point nil  ; disable mouse craziness
	  resize-mini-windows 'fit  ; Minibuffer resizing
	  ;; show-help-function nil          ; hide :help-echo text
	  uniquify-buffer-name-style 'forward
	  uniquify-strip-common-suffix nil
	  use-dialog-box nil              ; always avoid GUI
	  visible-cursor nil
	  x-stretch-cursor nil
	  ;; Defer jit font locking slightly to [try to] improve Emacs performance
	  jit-lock-defer-time nil
	  jit-lock-stealth-nice 0.1
	  jit-lock-stealth-time 0.2
	  jit-lock-stealth-verbose nil

	  ;; Smooth scrolling please!
	  ;; mouse-wheel-scroll-amount '(1 ((shift . 1)))
	  mouse-wheel-progressive-speed nil ;; no scrolling acceleration
	  mouse-wheel-follow-mouse t ;; scroll window under mouse
	  ;; keyword-base scrolling
	  scroll-step 1
	  scroll-conservatively 10000
	  scroll-margin 5
	  auto-window-vscroll nil
	  ;; No beeping or blinking
	  ring-bell-function #'ignore
	  visible-bell nil)


(setq-default tab-width 4
			  indent-tabs-mode t)

(put 'narrow-to-region 'disabled nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; (global-hl-line-mode 1)
(desktop-save-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)

(cl-eval-when (compile)
  (require 'recentf))

(with-eval-after-load 'recentf
  (setq recentf-max-menu-items 500))
