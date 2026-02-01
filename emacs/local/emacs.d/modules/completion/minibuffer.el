;;; -*- lexical-binding: t; -*-

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package minibuffer
  :after evil
  :custom
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  ;; (minibuffer-prompt-properties
  ;;  '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (general-define-key
   :keymaps '(evil-ex-search-keymap
              evil-ex-completion-map
              evil-command-line-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map
              minibuffer-local-ns-map
              minibuffer-local-map)
   "C-t" 'next-line-or-history-element
   "C-c" 'previous-line-or-history-element
   "M-c" 'beginning-of-buffer
   "M-t" 'end-of-buffer
   "C-S-t" 'scroll-up-command
   "C-S-c" 'scroll-down-command
   "C-h" 'left-char
   "C-n" 'right-char

   "<escape>" 'do-evil-escape-abort
   "C-q" 'minibuffer-keyboard-quit
   "M-RET" 'minibuffer-force-complete-and-exit

   "C-d" 'kill-whole-line
   "C-u" 'kill-line
   "C-$" 'move-end-of-line
   "C-a" 'move-end-of-line
   "C-0" 'move-beginning-of-line
   "C-i" 'move-beginning-of-line
   "C-w" 'forward-word
   "C-b" 'backward-word
   "C-p" 'yank)

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

(use-package all-the-icons
  :ensure t
  :disabled t
  :if (display-graphic-p))

(use-package nerd-icons-completion
  :ensure t
  :disabled t
  :after minibuffer
  :if (display-graphic-p)
  :config
  (nerd-icons-completion-mode))

(provide 'do-minibuffer)

;;; minibuffer.el ends here
