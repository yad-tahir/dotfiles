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
   "<f1>" 'help-command
   "M-ESC" 'do-evil-escape-abort)

  (general-define-key
   :states 'emacs
   ;; Basic Navigation
   "C-h" 'evil-backward-char
   "C-n" 'evil-forward-char
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
   "<C-escape>" 'evil-execute-in-normal-state
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
   "C-d" 'evil-delete
   "C-u" 'evil-change
   "C-:" 'evil-ex
   "C-;" 'evil-ex)

  ;; Normal and Visual
  (general-define-key
   ;; Don't but buffer modifier in the override keymap, e.g. evil-change
   ;; In some modes, you need to override them, e.g. magit
   :keymaps 'override
   :states '(normal visual)
   "<mouse-3>" 'ignore
   "<mouse-2>" 'ignore

   ;; Navigation
   "h" 'evil-backward-char
   "n" 'evil-forward-char
   "c" 'evil-previous-visual-line
   "t" 'evil-next-visual-line
   "gc" 'evil-previous-line
   "gt" 'evil-next-line

   ;; Searching
   "/" 'evil-ex-search-forward
   "?" 'evil-ex-search-backward
   "*" 'evil-ex-search-word-forward
   "g*" 'evil-ex-search-unbounded-word-forward
   "gn" 'evil-next-match
   "gh" 'evil-previous-match
   "N" 'evil-ex-search-next
   "H" 'evil-ex-search-previous

   ;; Scrolling
   "C" 'evil-scroll-page-up
   "T" 'evil-scroll-page-down

   ":" 'evil-ex
   ";" 'evil-ex)

  (general-define-key
   :states '(normal visual)
   "s" 'nil ;; Used for searching instead
   "l" 'nil ;; Used as a prefix for evil-local keybindings
   "SPC l" 'nil ;; Used as a prefix for mode-local keybindings
   "j" 'nil
   "x" 'nil
   "J" 'nil
   "f" 'nil
   "F" 'nil
   "," 'nil
   "g," 'nil
   "gj" 'nil
   "gJ" 'nil
   "g:" 'nil
   "'" 'nil

   "C-c" 'ignore
   "C-t" 'ignore
   "C-n" 'ignore
   "C-h" 'ignore

   "X" 'do-evil-fixup-whitespace

   ;; Primary operations
   "d" 'evil-delete
   "u" 'evil-change
   "U" 'evil-change-line
   "I" 'do-evil-insert
   "A" 'do-evil-append

   "z" 'undo
   "C-Z" 'redo
   "M-z" 'undo-tree-visualize
   "Z" 'undo-tree-visualize

   "=" 'do-evil-indent
   "g=" 'evil-indent

   "]" '(:ignore t :which-key "move-forward")
   "][" 'evil-forward-section-begin
   "]]" 'evil-forward-section-end
   "]s" 'evil-forward-sentence-begin
   "]S" 'evil-forward-sentence-end
   "]p" 'evil-forward-paragraph
   "]m" 'do-evil-forward-motion

   "[" '(:ignore t :which-key "move-backward")
   "[[" 'evil-backward-section-begin
   "[]" 'evil-backward-section-end
   "[s" 'evil-backward-sentence-begin
   "[S" 'evil-backward-sentence-end
   "[p" 'evil-backward-paragraph
   "[m" 'do-evil-backward-motion

   "SPC l" '(:ignore t :which-key "local")
   "SPC lq" 'evil-quit
   "SPC lw" 'evil-save-and-quit

   "SPC g" '(:ignore t :which-key "go")
   "lg" '(:ignore t :which-key "jump")
   "lgh" 'evil-jump-backward
   "lgn" 'evil-jump-forward
   "lgH" 'goto-last-change
   "lgN" 'goto-last-change-reverse
   "lgt" 'evil-jump-to-tag
   "lgc" 'evil-goto-column
   "lgd" 'evil-goto-definition
   "lgl" 'evil-show-jumps
   "lgs" 'evil-jump-backward-swap
   "l-" 'do-evil-narrow
   "l+" 'widen)

  (general-define-key
   :states 'visual
   "@" '(lambda ()
		  (interactive)
		  ;; Make micros more useful in visual mode
		  ;; @TODO: Does not work in Visual Block Mode
		  (evil-ex "'<,'>normal @")))

  (general-define-key
   :states 'motion
   "t" 'nil
   "T" 'nil
   "k" 'nil
   "K" 'nil
   "l" 'nil
   "L" 'nil
   "x" 'nil
   "X" 'nil
   "N" 'nil
   "C" 'nil
   "T" 'nil
   "H" 'nil
   "M" 'nil
   "L" 'nil
   "'" 'nil
   "\"" 'nil

   "j" 'evil-find-char-to
   "J" 'evil-find-char-to-backward
   "f" 'evil-find-char
   "F" 'evil-find-char-backward
   "," 'evil-repeat-find-char
   "g," 'evil-repeat-find-char-reverse

   ;; Make sure basic navigation matches the normal state
   "h" 'evil-backward-char
   "n" 'evil-forward-char
   "c" 'evil-previous-visual-line
   "t" 'evil-next-visual-line
   "gc" 'evil-previous-line
   "gt" 'evil-next-line

   ;; Jumping
   "gj" 'evil-jump-backward
   "gJ" 'evil-jump-forward
   "g:" 'goto-last-change-reverse
   "'" 'evil-goto-mark)

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
   "C-h" 'left-char
   "C-n" 'right-char
   "M-c" 'beginning-of-buffer
   "M-t" 'end-of-buffer

   "C-p" 'yank
   "C-w" 'forward-word
   "C-b" 'backward-word
   "C-$" 'move-end-of-line
   "C-0" 'move-beginning-of-line
   "C-i" 'backward-kill-word
   "C-a" 'kill-word
   "C-u" 'kill-line
   "C-d" 'kill-whole-line
   "<escape>" 'do-evil-escape-abort
   "C-k" 'describe-key)

  (general-define-key
   :keymaps 'with-editor-mode-map
   :states 'normal
   [remap evil-save-and-quit] 'with-editor-finish
   [remap evil-quit] 'with-editor-cancel)

  (general-define-key
   :keymaps 'button-map
   "<return>" 'push-button
   "<space>" 'push-button
   "<mouse-1>" 'push-button)

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

  ;; Variables
  (setq-default evil-symbol-word-search t) ;; make '*' more useful
  (setq evil-want-C-u-scroll nil
		evil-want-C-i-jump nil
		evil-want-Y-yank-to-eol t
		;; If I want to select \n, then keep it \n in visual state instead of
		;; selecting the first character in the next line
		evil-want-visual-char-semi-exclusive nil
		;; No need for t if the cursor has a box shap
		evil-move-beyond-eol nil
		evil-ex-search-persistent-highlight t
		evil-move-cursor-back t
		evil-magic t
		evil-indent-convert-tabs t
		evil-auto-indent t
		evil-shift-width 4
		evil-jumps-cross-buffers t
		evil-shift-round t
		evil-insert-skip-empty-lines t
		evil-echo-state nil
		evil-kbd-macro-suppress-motion-error t
		evil-mode-line-format nil
		evil-cross-lines t
		;; evil-ex-substitute-global t
		;; More vim-like behavior
		evil-ex-search-vim-style-regexp t
		;; evil-search-wrap nil ;; Can be problematic with macros
		shift-select-mode t
		;; Setup the initial state for major modes should be normal
		evil-emacs-state-modes nil
		evil-motion-state-modes nil)

  ;; Theme
  (evil-put-property 'evil-state-properties 'insert   :tag "INSERT")
  (evil-put-property 'evil-state-properties 'visual   :tag "VISUAL")
  (evil-put-property 'evil-state-properties 'motion   :tag "MOTION")
  (evil-put-property 'evil-state-properties 'emacs    :tag "EMACS")
  (evil-put-property 'evil-state-properties 'replace  :tag "REPLACE")
  (evil-put-property 'evil-state-properties 'operator :tag "OPERATOR")

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

  ;; Functions
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

  (defun do-evil-escape-abort ()
	(interactive)
	(when (and delete-selection-mode
			   transient-mark-mode
			   mark-active)
	  (setq deactivate-mark  t))

	;; Terminate evil-search aggressively
	(unless defining-kbd-macro
	  (ignore-errors
		(evil-ex-search-abort))
	  (ignore-errors
		(evil-ex-delete-hl 'evil-ex-search))

	  ;; Abort other on-going edits if any
	  (ignore-errors
		(abort-recursive-edit))))

  ;; Start Evil mode
  (evil-mode 1))


(provide 'do-evil)
