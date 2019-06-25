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

(use-package fusion
  :load-path "~/.emacs.d/local-packages/fusion"
  :commands (fusion-join fusion-split fusion-resplit)
  :init
  (general-define-key
   :states '(normal visual)
   "k" 'fusion-join
   "K" 'fusion-split
   "lj" 'fusion-join
   "lJ" 'fusion-split))

(use-package evil-surround
  :ensure t
  :commands (evil-surround-edit
			 evil-surround-change
			 evil-surround-region
			 evil-surround-delete)
  :init
  (general-define-key
   :states 'normal
   "s" '(:ignore t :which-key "surround")
   "ss" 'evil-surround-edit
   "su" 'evil-surround-change
   "sd" 'evil-surround-delete)

  (general-define-key
   :states 'visual
   "s" '(:ignore t :which-key "surround")
   "ss" 'evil-surround-region
   "su" 'evil-surround-change
   "sd" 'evil-surround-delete)
  :config
  ;; Eliminate extra space between pairs
  (setq evil-surround-pairs-alist
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
  :hook ((prog-mode . turn-on-smartparens-mode))
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
		sp-highlight-wrap-overlay nil
		sp-highlight-wrap-tag-overlay nil)

  (general-define-key
   :keymaps 'smartparens-mode-map
   :states 'normal
   "M-n" 'sp-forward-slurp-sexp
   "M-h" 'sp-forward-barf-sexp
   "g M-n" 'sp-backward-slurp-sexp
   "g M-h" 'sp-backward-barf-sexp
   "ls" '(:ignore t :which-key "smart parens")
   "lsn" 'sp-forward-slurp-sexp
   "lsN" 'sp-forward-barf-sexp
   "lsh" 'sp-backward-slurp-sexp
   "lsH" 'sp-backward-barf-sexp
   "lsc" 'sp-up-sexp
   "lst" 'sp-down-sexp
   "lsu" 'sp-splice-sexp
   "lsj" 'sp-join-sexp))

;; (use-package evil-paredit
;;	:disabled t
;;	:ensure t
;;	:hook ((prog-mode . evil-paredit-mode)
;;		   (prog-mode . paredit-mode))
;;	:config
;;	;; Reset keymaps
;;	(setf (cdr evil-paredit-mode-map) nil)

;;	(general-define-key
;;	 :keymaps 'evil-paredit-mode-map
;;	 :states 'normal
;;	 ;; Bindings for basic movements
;;	 "lsn" 'paredit-forward-slurp-sexp
;;	 "lsN" 'paredit-forward-barf-sexp
;;	 "lsh" 'paredit-backward-slurp-sexp
;;	 "lsh" 'paredit-backward-barf-sexp
;;	 "M-n" 'paredit-forward-slurp-sexp
;;	 "M-h" 'paredit-forward-barf-sexp
;;	 "g M-h" 'paredit-backward-slurp-sexp
;;	 "g M-H" 'paredit-backward-barf-sexp
;;	 ;; Needed because we use 'u' instead of 'c'
;;	 "c" nil
;;	 "C" nil
;;	 "u" 'evil-paredit-change
;;	 "U" 'evil-paredit-change-line
;;	 "d" 'evil-paredit-delete
;;	 "D" 'evil-paredit-delete-line
;;	 "lsu" 'evil-change
;;	 "lsU" 'evil-change-line
;;	 "lsd" 'evil-delete
;;	 "lsD" 'evil-delete-line)

;;	(general-define-key
;;	 :keymaps 'evil-paredit-mode-map
;;	 :states 'visual
;;	 ;; Surrounding region
;;	 "ls(" 'paredit-wrap-round
;;	 "ls)" 'paredit-wrap-round
;;	 "ls]" 'paredit-wrap-square
;;	 "ls[" 'paredit-wrap-square
;;	 "ls{" 'paredit-wrap-curly
;;	 "ls}" 'paredit-wrap-curly
;;	 "ls>" 'paredit-wrap-angled
;;	 "ls<" 'paredit-wrap-angled))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :init
  (general-define-key
   :states 'visual
   "<return>" 'er/expand-region))

(use-package evil-nerd-commenter
  :ensure t
  :commands (evilnc-comment-operator)
  :init
  (general-define-key
   :states '(normal visual)
   "gk" 'evilnc-comment-operator
   "l;" 'evilnc-comment-operator))

(use-package evil-lion
  :defer t
  :ensure t
  :commands (evil-lion-left evil-lion-right)
  :init
  (general-define-key
   :states '(normal visual)
   "l<" #'evil-lion-left
   "l>" #'evil-lion-right)
  :config
  (setq evil-lion-left-align-key (kbd "l <")
		evil-lion-right-align-key (kbd "l >"))
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

;; (use-package aggressive-indent
;;   :hook ((prog-mode . aggressive-indent-mode)
;;		 (text-mode . aggressive-indent-mode)))

;; (use-package fold-this
;;   :commands 'fold-active-region-all
;;   :init
;;   (general-define-key
;;    :states 'visual
;;    "." #'fold-active-region-all))
