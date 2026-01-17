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

(use-package fusion
  :load-path "local-packages/fusion"
  :commands (fusion-join fusion-split fusion-resplit)
  :init
  (general-define-key
   :states '(normal visual)
   "k" 'fusion-join
   "K" 'fusion-split))

(use-package evil-surround
  :ensure t
  :commands (evil-surround-region
			 evil-surround-change
			 evil-Surround-change
			 evil-surround-delete)
  :init
  (general-define-key
   :states '(normal visual)
   "s" 'evil-surround-region
   "gs" 'evil-surround-change
   "S" 'evil-surround-delete)

  :config
  ;; Eliminate extra space between pairs
  (setq-default evil-surround-pairs-alist
				'((?\( . ("(" . ")"))
				  (?\[ . ("[" . "]"))
				  (?\{ . ("{" . "}"))
				  (?\) . ("(" . ")"))
				  (?\] . ("[" . "]"))
				  (?\} . ("{" . "}"))
				  (?# . ("#{" . "}"))
				  (?b . ("(" . ")"))
				  (?B . ("{" . "}"))
				  (?> . ("<" . ">"))
				  (?t . evil-surround-read-tag)
				  (?< . evil-surround-read-tag)
				  (?f . evil-surround-function))))

(use-package smartparens
  :ensure t
  :disabled t
  :hook ((prog-mode . turn-on-smartparens-mode))
  :config
  ;; Remove compiler warnings
  (eval-when-compile
	(declare-function sp-pair nil))

  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
		sp-highlight-wrap-overlay nil
		sp-highlight-wrap-tag-overlay nil)

  (sp-pair "\\'" nil :actions :rem)
  (sp-pair "\\\"" nil :actions :rem)

  (general-define-key
   :keymaps 'smartparens-mode-map
   :states 'normal
   "M-n" 'sp-forward-slurp-sexp
   "M-h" 'sp-forward-barf-sexp
   "g M-n" 'sp-backward-slurp-sexp
   "g M-h" 'sp-backward-barf-sexp))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :init
  (general-define-key
   :states 'visual
   ;; for Emacs GUI
   "<tab>" 'er/expand-region
   ;; for Emacs Terminal
   "TAB" 'er/expand-region))

(use-package evil-nerd-commenter
  :ensure t
  :commands (evilnc-comment-operator)
  :init
  (general-define-key
   :states '(normal visual)
   "x" 'evilnc-comment-operator))

(use-package evil-lion
  :defer t
  :ensure t
  :commands (evil-lion-left evil-lion-right)
  :init
  (general-define-key
   :states '(normal visual)
   "SPC l<" #'evil-lion-left
   "SPC l>" #'evil-lion-right)

  :config
  (setq evil-lion-left-align-key (kbd "SPC l <")
		evil-lion-right-align-key (kbd "SPC l >"))
  (evil-lion-mode))

(use-package drag-stuff
  :ensure t
  :commands (drag-stuff-up drag-stuff-down drag-stuff-left drag-stuff-right)
  :init
  (general-define-key
   :states '(normal visual)
   "M-c" 'drag-stuff-up
   "M-t" 'drag-stuff-down
   "M-n" 'drag-stuff-right
   "M-h" 'drag-stuff-left))

(use-package evil-replace-with-register
  :ensure t
  :disabled t
  :commands 'evil-replace-with-register
  :init
  (general-define-key
   :states '(normal visual)
   ;; Remove the Replace state with something more useful
   "r" 'evil-replace-with-register))

(use-package evil-numbers
  :ensure t
  :commands (evil-numbers/dec-at-pt evil-numbers/inc-at-pt)
  :init
  (general-define-key
   :states '(normal visual)
   "g-" 'evil-numbers/dec-at-pt
   "g+" 'evil-numbers/inc-at-pt))

(use-package evil-exchange
  :ensure t
  :commands (evil-exchange evil-exchange-cancel)
  :config
  (general-define-key
   :states '(normal visual)
   "SPC lx" 'evil-exchange
   "SPC lX" 'evil-exchange-cancel))

(use-package whitespace
  :config
  (add-hook 'before-save-hook #'(lambda ()
								  (when whitespace-mode
									(whitespace-cleanup)))))

(use-package evil-mc
  :ensure t
  :disabled t
  :after (evil)
  :config
  ;; Remove the existing keybindings
  (setcdr evil-mc-key-map nil)

  (general-define-key
   :keymaps 'evil-mc-key-map
   :states '(normal visual)
   :prefix "gr"
   "m" 'do-mc-make-cursor-here
   "M" 'evil-mc-make-all-cursors
   "*" 'evil-mc-make-all-cursors
   "I" 'evil-mc-make-cursor-in-visual-selection-beg
   "A" 'evil-mc-make-cursor-in-visual-selection-end

   "q" 'evil-mc-undo-all-cursors
   "<escape>" 'evil-mc-undo-all-cursors
   "d" 'evil-mc-undo-cursor-at-pos
   "z" 'evil-mc-undo-last-added-cursor

   "r" 'evil-mc-resume-cursors
   "SPC" 'evil-mc-pause-cursors)

  (general-define-key
   :keymaps 'evil-mc-key-map
   :states '(normal visual)
   "gr" '(:ignore t :which-key "cursors"))

  (general-define-key
   :keymaps 'evil-mc-key-map
   :states 'visual
   "gr" '(:ignore t :which-key "cursors")
   "A" 'do-evil-mc-mark-visual-end
   "I" 'do-evil-mc-mark-visual-beg)

  (defun do-mc-make-cursor-here ()
	(interactive)
	(evil-mc-make-cursor-here)
	(evil-mc-pause-cursors))

  ;; Make sure the cursors are not over hidden, e.g. evil-ex-search
  (setq evil-mc-cursor-overlay-priority 1000)

  (set-face-background 'evil-mc-cursor-default-face "orange")
  (set-face-foreground 'evil-mc-cursor-default-face "black")

  ;; (add-hook 'evil-mc-before-cursors-created 'evil-mc-pause-cursors)
  ;; (add-hook 'evil-mc-after-cursors-created 'evil-mc-pause-cursors)
  (global-evil-mc-mode +1))

(use-package evil-insert-plus
  :commands (evil-insert-plus evil-append-plus)
  :init
  (general-define-key
   :states '(normal visual)
   "A" 'evil-append-plus
   "I" 'evil-insert-plus))

(use-package evil-closest-delimiter
  :commands (evil-closest-inner-delimiter evil-closest-a-delimiter)
  :init
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "m" 'evil-closest-inner-delimiter)

  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "m" 'evil-closest-a-delimiter))

(provide 'do-text-adjust)
