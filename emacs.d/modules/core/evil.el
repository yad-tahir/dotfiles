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

(use-package evil
  :ensure t
  :demand t
  :defines (evil-want-Y-yank-to-eol chocolate-theme-element+6)
  :init
  (setq-default evil-search-module 'evil-search)

  :config
  (general-define-key
   "C-c" 'nil
   "<f1>" 'help-command)

  (general-define-key
   :states 'emacs
   ;; Basic Navigation
   "C-h" 'backward-char
   "C-n" 'forward-char
   "C-c" 'evil-previous-line
   "C-t" 'evil-next-line
   ":" 'evil-ex
   ";" 'evil-ex
   "<escape>" 'evil-normal-state
   "\\" 'evil-normal-state)

  (general-define-key
   :states 'insert
   ;; Don't create another normal mode from the insert mode. Thus, we
   ;; will only allow basic navigation here.
   "<tab>" 'tab-to-tab-stop
   "C-i" 'evil-beginning-of-line
   "C-0" 'evil-beginning-of-line
   "C-a" 'evil-end-of-line
   "C-$" 'evil-end-of-line
   "C-w" 'evil-forward-word-begin
   "C-b" 'evil-backward-word-begin
   "C-n" 'right-char
   "C-h" 'left-char
   "C-c" 'evil-previous-visual-line
   "C-t" 'evil-next-visual-line
   "C-f" 'evil-find-char
   "C-j" 'evil-find-char-to
   "C-:" 'evil-ex
   "C-;" 'evil-ex)

  ;; Motion
  (general-define-key
   :states 'motion
   :keymaps 'override
   "h" 'backward-char
   "n" 'forward-char
   "c" 'evil-previous-visual-line
   "t" 'evil-next-visual-line
   "gc" 'evil-previous-line
   "gt" 'evil-next-line
   "/" 'evil-ex-search-forward
   "?" 'evil-ex-search-backward
   "*" 'evil-ex-search-word-forward
   "g*" 'evil-ex-search-unbounded-word-forward
   "gj" 'evil-find-char-to-backward
   "gn" 'evil-next-match
   "gh" 'evil-previous-match)

  (general-define-key
   :states 'motion
   "s" 'nil
   "j" 'nil
   "H" 'nil
   "N" 'nil
   "C" 'nil
   "T" 'nil
   "k" 'nil
   "K" 'nil
   "l" 'nil
   "L" 'nil
   "S" 'nil)

  ;; Normal
  (general-define-key
   :states 'normal
   :keymaps 'override
   "<mouse-3>" '(lambda () (interactive) (message "no mouse please!"))
   "<mouse-2>" '(lambda () (interactive) (message "no mouse please!"))
   "h" 'backward-char
   "n" 'forward-char
   "c" 'evil-previous-visual-line
   "t" 'evil-next-visual-line
   "gc" 'evil-previous-line
   "gt" 'evil-next-line
   "C-n" 'evil-ex-search-next
   "C-h" 'evil-ex-search-previous
   "gp" '(lambda()(interactive)(evil-insert-newline-below)(yank))
   "gP" '(lambda()(interactive)(evil-insert-newline-above)(yank))
   "C-p" '(lambda ()(interactive)
			(let ((evil-this-register ?0))
			  (call-interactively 'evil-paste-after)))
   "'" 'evil-goto-mark
   ":" 'evil-ex
   ";" 'evil-ex
   "j" 'evil-find-char-to
   "J" 'evil-find-char-to-backward
   ;; Jumping motions
   "g:" 'goto-last-change-reverse
   "gj" 'evil-jump-backward
   "gJ" 'evil-jump-forward
   "gH" 'evil-window-top
   "gM" 'evil-window-middle
   "gL" 'evil-window-bottom
   "xg" '(:ignore t :which-key "jump")
   "xgh" 'evil-jump-backward
   "xgn" 'evil-jump-forward
   "xgH" 'goto-last-change
   "xgN" 'goto-last-change-reverse
   "xgt" 'evil-jump-to-tag
   "xgc" 'evil-goto-column
   "xgd" 'evil-goto-definition
   "xgl" 'evil-show-jumps
   "xgs" 'evil-jump-backward-swap)

  (general-define-key
   :states 'normal
   "z" 'undo
   "C-Z" 'redo
   "M-z" '(lambda () (interactive) (do-make-frame)(undo-tree-visualize))
   "Z" '(lambda () (interactive) (do-make-frame)(undo-tree-visualize))
   "H" 'ignore
   "N" 'ignore
   "C-c" 'evil-scroll-page-up
   "C-t" 'evil-scroll-page-down
   "s" 'nil ;; Used for searching instead
   "U" 'evil-change-line
   "," 'widen ;; @TASK: To evil operator
   "l" 'nil
   "x" 'nil
   "u" 'evil-change
   "d" 'evil-delete)

  ;; Visual
  (general-define-key
   :states 'visual
   :keymaps 'override
   "<mouse-3>" '(lambda () (interactive) (message "no mouse please!"))
   "<mouse-2>" '(lambda () (interactive) (message "no mouse please!"))
   "h" 'backward-char
   "n" 'forward-char
   "c" 'evil-previous-visual-line
   "t" 'evil-next-visual-line
   "gc" 'evil-previous-line
   "gt" 'evil-next-line
   "C-n" 'evil-ex-search-next
   "C-h" 'evil-ex-search-previous
   "gp" '(lambda()(interactive)(evil-insert-newline-below)(yank))
   "gP" '(lambda()(interactive)(evil-insert-newline-above)(yank))
   "C-p" '(lambda ()(interactive)
			(let ((evil-this-register ?0))
			  (call-interactively 'evil-paste-after)))
   "'" 'evil-goto-mark
   ":" 'evil-ex
   ";" 'evil-ex
   "j" 'evil-find-char-to
   "J" 'evil-find-char-to-backward
   "xi" 'do-evil-insert-to-motion
   "xa" 'do-evil-append-to-motion)

  (general-define-key
   :states 'visual
   "z" 'undo
   "C-Z" 'redo
   "M-z" '(lambda () (interactive) (do-make-frame)(undo-tree-visualize))
   "Z" '(lambda () (interactive) (do-make-frame)(undo-tree-visualize))
   "H" 'ignore
   "N" 'ignore
   "C-c" 'evil-scroll-page-up
   "C-t" 'evil-scroll-page-down
   "U" 'evil-change-line
   "," 'narrow-to-region
   "gk" 'capitalize-region
   "@" '(lambda ()  (interactive)
		  ;; Make micros more useful in visual mode
		  ;; @TODO: Does not work in Visual Block Mode
		  (evil-ex "'<,'>normal @"))
   "s" 'nil ;; Used for searching instead
   "u" 'nil
   "x" 'nil
   "l" 'nil
   "d" 'nil)

  ;; Other
  (general-define-key
   :keymaps '(evil-ex-search-keymap
			  evil-ex-completion-map
			  minibuffer-local-completion-map
			  minibuffer-local-must-match-map
			  minibuffer-local-isearch-map
			  minibuffer-local-ns-map
			  minibuffer-local-map)
   "C-c" 'previous-history-element
   "C-t" 'next-history-element
   "C-n" 'forward-word
   "C-w" 'forward-word
   "C-h" 'backward-word
   "C-b" 'backward-word
   "<escape>" '(lambda ()
				 (interactive)
				 (ignore-errors
				   (if (and delete-selection-mode transient-mark-mode mark-active)
					   (setq deactivate-mark  t)
					 (when (get-buffer "*Completions*")
					   (delete-windows-on "*Completions*"))
					 (abort-recursive-edit))))
   "C-p" 'yank
   "C-4" 'move-end-of-line
   "C-0" 'move-beginning-of-line
   "C-a" 'backward-kill-word
   "C-i" 'kill-word
   "C-u" 'kill-line
   "C-d" 'kill-whole-line
   "C-k" 'describe-key)

  (general-define-key
   :keymaps 'with-editor-mode-map
   :prefix "l"
   :states 'normal
   "q" 'with-editor-finish
   "<escape>" 'with-editor-cancel)

  (setq-default evil-symbol-word-search t) ;; make * and  more useful

  (setq evil-want-C-u-scroll nil
		evil-want-C-i-jump nil
		evil-want-visual-char-semi-exclusive t
		evil-want-Y-yank-to-eol t
		evil-move-cursor-back t
		evil-magic t
		evil-indent-convert-tabs t
		evil-auto-indent t
		evil-shift-width 4
		evil-jumps-cross-buffers t
		;; indent-tabs-mode t
		evil-shift-round t
		evil-insert-skip-empty-lines t
		evil-echo-state nil
		evil-kbd-macro-suppress-motion-error t
		evil-mode-line-format nil
		;; evil-ex-substitute-global t
		;; More vim-like behavior
		evil-ex-search-vim-style-regexp t
		;; evil-search-wrap nil ;; Can be problematic with macros
		shift-select-mode t ;; Don't activate mark on shift-click
		;; Setup the initial state for major modes should be normal
		evil-emacs-state-modes nil
		evil-motion-state-modes nil)

  ;; Theme
  (evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
  (evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
  (evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
  (evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
  (evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
  (evil-put-property 'evil-state-properties 'operator :tag " OPERATOR ")

  (setq evil-default-cursor `(box)
		evil-insert-state-cursor `((bar . 5))
		evil-normal-state-cursor `(box)
		evil-visual-state-cursor `(box)
		evil-emacs-state-cursor `(box)
		evil-replace-state-cursor `((bar . 5))
		evil-operator-state-cursor `(box)
		evil-motion-state-cursor `(box))

  (with-eval-after-load 'chocolate-theme-theme
	(set-face-attribute 'evil-ex-substitute-replacement nil
						:background nil :foreground chocolate-theme-element+6))
  ;; Minimize keymaps overriding
  (setq evil-overriding-maps nil
		evil-want-integration nil
		evil-intercept-maps nil
		evil-pending-intercept-maps nil
		evil-pending-overriding-maps nil)
  ;; Subvert evil-operation.el overrides (dired, ibuffer etc.)
  (advice-add 'evil-make-overriding-map :override 'ignore)
  (advice-add 'evil-make-intercept-map  :override 'ignore)
  (advice-add 'evil-add-hjkl-bindings   :override 'ignore)

  (dolist (mode '(text-mode help-mode debugger-mode))
	(evil-set-initial-state mode 'normal))

  (defun do--evil-travel-state (org-func &rest args)
	"Make default state normal when traveling between windows. This is useful to
avoid navigating with the insert state."

	(unless (memq evil-state '(normal motion emacs))
	  (evil-normal-state +1))
	(apply org-func args))
  (advice-add 'windmove-do-window-select :around
			  'do--evil-travel-state-on-windmove)

  ;; Don't move cursor on indent
  (advice-add 'evil-indent
			  :around '(lambda (org-func &rest args)
						 (save-excursion (apply org-func args))))


;;;###autoload
  (evil-define-operator do-evil-insert-to-motion (beg end)
	"Switch to Insert state at the beginning of the ongoing motion"
	(interactive "<r>")
	(ignore end)
	(goto-char beg)
	(evil-insert-state))

;;;###autoload
  (evil-define-operator do-evil-append-to-motion (beg end)
	"Switch to Insert state at the end of ongoing motion"
	(interactive "<r>")
	(ignore beg)
	(goto-char end)
	(evil-insert-state))

  ;; start Evil mode
  (evil-mode))
