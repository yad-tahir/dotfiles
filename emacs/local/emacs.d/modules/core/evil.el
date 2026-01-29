;;; package -- evil settings -*- lexical-binding: t; -*-

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

;;; Commentary:
;; My Emacs/Evil settings

;;; Code:

(use-package evil
  :ensure t
  :demand t
  :functions (evil-indent@evil-indent-fixed-mouse
              previous-history-element@previous-history-end-element
              next-history-element@next-history-end-element
              evil-ex-search-abort do--evil-search-region
              evil-exit-visual-state evil-ex-search-next
              evil-push-search-history evil-ex-make-search-pattern
              evil-set-initial-state do-evil-search-region-backward
              do-evil-search-region-forward evil-ex-delete-hl)
  :init
  (setq-default evil-search-module 'evil-search)

  :config
  ;; global keybinding regardless whether its evil-mode or not
  (general-define-key
   "C-c" 'nil
   "<f1>" 'help-command
   "M-ESC" 'do-evil-escape-abort

   "C-c" 'ignore
   "C-t" 'ignore
   "C-n" 'ignore
   "C-h" 'ignore
   "C-x" 'ignore
   "C-z" 'ignore

   ;; no mouse junks!
   "<mouse-3>" 'ignore
   "<mouse-2>" 'ignore)

  ;; Keep emacs state's mapping is unmodified as possible
  (general-define-key
   :states 'emacs
   ":" 'evil-ex
   ";" 'evil-ex
   ;; "<escape>" 'evil-normal-state ;; cause a problem for vundo
   "\\" 'evil-normal-state)

  (general-define-key
   :states 'insert
   ;; Don't create another normal mode from the insert mode.
   "TAB" 'tab-to-tab-stop

   "C-h" 'evil-backward-char
   "C-n" 'evil-forward-char
   "C-c" 'evil-previous-line
   "C-t" 'evil-next-line

   "C-z" 'evil-undo
   "C-Z" 'evil-redo

   "C-:" 'evil-ex
   "C-;" 'evil-ex)

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
   "SPC" 'nil
   "'" 'nil
   "\"" 'nil

   ;; Basic navigation
   "h" 'evil-backward-char
   "n" 'evil-forward-char
   "c" 'evil-previous-visual-line
   "t" 'evil-next-visual-line
   "gc" 'evil-previous-line
   "gt" 'evil-next-line

   ;; Scrolling
   "C" 'evil-scroll-page-up
   "T" 'evil-scroll-page-down

   ;; Searching
   "/" 'evil-ex-search-forward
   "?" 'evil-ex-search-backward
   "gn" 'evil-next-match
   "gh" 'evil-previous-match
   "g^" 'evil-first-non-blank
   "N" 'evil-ex-search-next
   "H" 'evil-ex-search-previous

   "]" '(:ignore t :which-key "move-forward")
   "][" 'evil-forward-section-begin
   "]]" 'evil-forward-section-end
   "]s" 'evil-forward-sentence-begin
   "]S" 'evil-forward-sentence-end
   "]p" 'evil-forward-paragraph
   "]m" 'do-evil-forward-motion
   "]." 'do-evil-forward-motion

   "[" '(:ignore t :which-key "move-backward")
   "[[" 'evil-backward-section-begin
   "[]" 'evil-backward-section-end
   "[s" 'evil-backward-sentence-begin
   "[S" 'evil-backward-sentence-end
   "[p" 'evil-backward-paragraph
   "[m" 'do-evil-backward-motion
   "[." 'do-evil-backward-motion

   ;; Jumping
   "gj" 'evil-jump-backward
   "gJ" 'evil-jump-forward
   "g:" 'goto-last-change-reverse
   "'" 'evil-goto-mark

   "gy" '(:ignore t :which-key "jump")
   "gyh" 'evil-jump-backward
   "gyn" 'evil-jump-forward
   "gyH" 'goto-last-change
   "gyN" 'goto-last-change-reverse
   "gyt" 'evil-jump-to-tag
   "gyc" 'evil-goto-column
   "gyd" 'evil-goto-definition

   ;; Motion state specials
   "j" 'evil-find-char-to
   "J" 'evil-find-char-to-backward
   "f" 'evil-find-char
   "F" 'evil-find-char-backward
   "," 'evil-repeat-find-char
   "g," 'evil-repeat-find-char-reverse)

  (general-define-key
   :states 'normal
   "C-c" 'ignore
   "C-t" 'ignore
   "C-n" 'ignore
   "C-h" 'ignore
   "C-z" 'ignore

   ;; Reset/remove conflicting bindings
   "c" nil
   "C" nil
   "s" nil
   "S" nil
   "x" nil
   "X" nil
   "," nil
   "'" nil
   "g:" nil
   "g," nil
   "gj" nil
   "gJ" nil
   "gt" nil
   "gT" nil

   ;; Primary operations
   "d" 'evil-delete
   "u" 'evil-change
   "U" 'evil-change-line
   "k" 'evil-join
   "K" 'evil-join-whitespace
   "z" 'evil-undo
   "Z" 'evil-redo
   "C-z" 'ignore

   ;; Paste
   "r" 'do-evil-replace
   "gp" 'do-evil-paste-next-line
   "gP" 'do-evil-paste-previous-line

   ;; Whitespace/Indentation
   "gk" 'evil-join-whitespace

   ;; Text case
   "gu" 'evil-downcase
   "gU" 'evil-upcase

   "SPC c" 'compile ;; enter compile mode
   "gx" 'do-evil-narrow
   "gX" 'widen)

  (general-define-key
   :states 'visual
   ;; Reset/remove conflicting bindings
   "gf" nil
   "z" nil
   "R" nil ;; replaced by U

   "u" 'evil-change
   "U" 'evil-change-line
   "*" #'do-evil-search-region-forward
   "#" #'do-evil-search-region-backward)

  ;; Normal and Visual
  (general-define-key
   ;; Don't but buffer modifier in the override keymap, e.g. evil-change
   ;; In some modes, you need to override them, e.g. magit
   :keymaps 'override
   ;; Must be both normal and visual since this is on a separate keymap. If I
   ;; use only normal, then these keybindings will be missing over the visual
   ;; of override. It will then go to other underline keybindings.
   :states '(normal visual)
   "\\" 'evil-emacs-state
   ":" 'evil-ex
   ";" 'evil-ex)

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
   "C-a" 'kill-word
   "C-u" 'kill-line
   "C-d" 'kill-whole-line
   "<escape>" 'do-evil-escape-abort
   "C-k" 'describe-key)

  (general-define-key
   :keymaps 'with-editor-mode-map
   :states 'normal
   "SPC lw" 'with-editor-finish
   "SPC lq" 'with-editor-cancel)

  (general-define-key
   :keymaps 'button-map
   "<RET>" 'push-button
   "<space>" 'push-button
   "<mouse-1>" 'push-button)

  ;; Minimize keymaps overriding
  (setq evil-overriding-maps nil
        ;; Needed for company-mode.
        evil-want-integration t
        evil-intercept-maps nil
        evil-pending-intercept-maps nil
        evil-pending-overriding-maps nil)

  ;; Subvert evil-operation.el overrides
  (advice-add 'evil-make-overriding-map :override 'ignore)
  (advice-add 'evil-make-intercept-map  :override 'ignore)
  (advice-add 'evil-add-hjkl-bindings   :override 'ignore)

  ;; Variables
  (setq-default evil-symbol-word-search t) ;; make '*' more useful

  (setq evil-want-C-u-scroll nil
        evil-want-C-d-scroll nil
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t
        ;; No need for t if the cursor has a box shap
        evil-move-beyond-eol nil
        evil-ex-search-persistent-highlight t
        evil-move-cursor-back t
        evil-magic t
        evil-indent-convert-tabs t
        evil-auto-indent t
        evil-shift-width 4
        evil-jumps-cross-buffers nil
        evil-shift-round t
        evil-insert-skip-empty-lines t
        evil-echo-state nil
        evil-kbd-macro-suppress-motion-error t
        ;; evil-move-beyond-eol t
        evil-mode-line-format nil
        evil-cross-lines t
        evil-lookup-func #'(lambda() (call-interactively 'eldoc-doc-buffer))
        ;; evil-ex-substitute-global t
        ;; More vim-like behavior
        evil-ex-search-vim-style-regexp t
        ;; evil-search-wrap nil ;; Can be problematic with macros
        shift-select-mode t
        evil-respect-visual-line-mode t
        ;; Setup the initial state for major modes should be normal
        evil-emacs-state-modes nil
        evil-motion-state-modes nil)

  (setq evil-default-cursor `(box)
        evil-insert-state-cursor `((bar . 2))
        evil-normal-state-cursor `(box)
        evil-visual-state-cursor `(box)
        evil-emacs-state-cursor `(box)
        evil-replace-state-cursor `((bar . 2))
        evil-operator-state-cursor `(box)
        evil-undo-system 'undo-redo
        evil-redo-function 'undo-redo
        evil-motion-state-cursor `(box))

  (dolist (mode '(text-mode help-mode debugger-mode message-mode))
    (evil-set-initial-state mode 'normal))

  ;; Functions
  (defun do--evil-search-region (direction)
    "Extract the text of the current region and execute evil-ex-search."
    (interactive)
    (if (use-region-p)  ;; Check if the region is active
        (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
               (regex (regexp-quote selected-text)))
          (progn
            (unless (equal regex (car evil-ex-search-history))
              (push regex evil-ex-search-history))

            (setq evil-ex-search-count 1
                  evil-ex-search-direction direction
                  evil-ex-search-pattern
                  (let (evil-ex-search-vim-style-regexp)
                    (evil-ex-make-search-pattern regex))
                  evil-ex-search-offset nil
                  evil-ex-last-was-search t)
            (evil-push-search-history regex direction)
            (evil-ex-delete-hl 'evil-ex-search)
            (evil-ex-search-next)
            (evil-exit-visual-state)))
      (message "No region selected.")))

  (defun do-evil-search-region-backward ()
    "Extract the text of the current region and search background."
    (interactive)
    (do--evil-search-region 'backward))

  (defun do-evil-search-region-forward ()
    "Extract the text of the current region and search forward."
    (interactive)
    (do--evil-search-region 'forward))

  ;; When we travel through the history, put the cursor at the end of the line
  (with-eval-after-load 'simple
    (define-advice previous-history-element (:after (&rest _args) previous-history-end-element)
      "Move to end of line after previous history element."
      (move-end-of-line nil))

    (define-advice next-history-element (:after (&rest _args) next-history-end-element)
      "Move to end of line after next history element."
      (move-end-of-line nil)))

  ;; Don't move cursor on indent
  (define-advice evil-indent
      (:around (orig-func &rest args) evil-indent-fixed-mouse)
    (save-excursion (apply orig-func args)))

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

;;; evil.el ends here
