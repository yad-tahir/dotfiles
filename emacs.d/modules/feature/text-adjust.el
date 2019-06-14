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

(with-eval-after-load 'evil

  (declare-function do-join-region nil)
  (declare-function do-wrap-region nil)
  (declare-function do-rewrap-region nil)
  (declare-function do--expand-region-init nil)

  (general-define-key
   :states '(normal visual)
   "SPC ljj" 'do-join-region
   "SPC ljJ" 'do-wrap-region
   "SPC ljr" 'do-rewrap-region)

  (evil-define-operator do-join-region (beginning end)
	"Replaces new line chars in region by single spaces.

The second argument BEGINNING indicates the starting position of the
region. Passing nil makes the region starts
from (region-beginning).

The third argument END indicates the starting position of the
region. Passing nil makes the region starts from (region-end).

	This evil operator is the reverse of 'do-wrap-region'"

	;; Check the arguments and assign default values
	(unless beginning
	  (setq beginning (region-beginning)))
	(unless end
	  (setq end (region-end)))
	(when (< beginning end)
	  ;; Indent before filling is always a good idea
	  (evil-indent beginning end)
	  ;; Ask Emacs to join the region by setting the fill-column var to max int
	  (evil-join beginning end)
	  ;; (let ((fill-column most-positive-fixnum))
	  ;; 	(fill-region beginning end))
	  (evil-indent beginning end)))

  (evil-define-operator do-wrap-region (beginning end)
	"Fills the selected region and makes it indent.

The second argument BEGINNING indicates the starting position of the
region. Passing nil makes the region starts
from (region-beginning).

The third argument END indicates the starting position of the
region. Passing nil makes the region starts from (region-end)."

	(unless beginning
	  (setq beginning (region-beginning)))
	(unless end
	  (setq end (region-end)))
	(when (< beginning end)
	  (evil-indent beginning end)
	  (fill-region beginning end)
	  (evil-indent beginning end)))

  (evil-define-operator do-rewrap-region (beginning end)
	"Joins region then fills it again. This function is useful to re-wrap
a badly fragmented region.

The second argument BEGINNING indicates the starting position of the
region. Passing nil makes the region starts
from (region-beginning).

The third argument END indicates the starting position of the
region. Passing nil makes the region starts from (region-end)."
	(interactive)
	(do-join-region beginning end)
	(do-wrap-region beginning end))

;;; Third-party packages

  (use-package evil-surround
	:ensure t
	:defer 10
	:init
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
			(?f . evil-surround-function)))
	:config
	(general-define-key
	 :states '(normal)
	 :prefix "SPC l"
	 "s" '(:ignore t :which-key "surround")
	 "ss" 'evil-surround-edit
	 "si" 'evil-surround-edit
	 "sa" 'evil-surround-edit
	 "su" 'evil-surround-change
	 "sd" 'evil-surround-delete)

	(general-define-key
	 :states '(visual)
	 :prefix "SPC l"
	 "s" '(:ignore t :which-key "surround")
	 "ss" 'evil-surround-region
	 "si" 'evil-surround-region
	 "sa" 'evil-surround-region
	 "su" 'evil-surround-change
	 "sd" 'evil-surround-delete)

	(general-define-key
	 :states '(normal)
	 ;; :prefix "g"
	 "s" '(:ignore t :which-key "surround")
	 "ss" 'evil-surround-edit
	 "si" 'evil-surround-edit
	 "sa" 'evil-surround-edit
	 "su" 'evil-surround-change
	 "sd" 'evil-surround-delete)

	(general-define-key
	 :states '(visual)
	 ;; :prefix "g"
	 "s" '(:ignore t :which-key "surround")
	 "ss" 'evil-surround-region
	 "si" 'evil-surround-region
	 "sa" 'evil-surround-region
	 "su" 'evil-surround-change
	 "sd" 'evil-surround-delete))

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
	 "SPC ls" '(:ignore t :which-key "smart parens")
  	 "SPC lsn" 'sp-forward-slurp-sexp
  	 "SPC lsN" 'sp-forward-barf-sexp
  	 "SPC lsh" 'sp-backward-slurp-sexp
  	 "SPC lsH" 'sp-backward-barf-sexp
  	 "SPC lsc" 'sp-up-sexp
  	 "SPC lst" 'sp-down-sexp
  	 "SPC lsu" 'sp-splice-sexp
	 "SPC lsj" 'sp-join-sexp))

  ;; (use-package evil-paredit
  ;; 	:disabled t
  ;; 	:ensure t
  ;; 	:hook ((prog-mode . evil-paredit-mode)
  ;; 		   (prog-mode . paredit-mode))
  ;; 	:config
  ;; 	;; Reset keymaps
  ;; 	(setf (cdr evil-paredit-mode-map) nil)

  ;; 	(general-define-key
  ;; 	 :keymaps 'evil-paredit-mode-map
  ;; 	 :states 'normal
  ;; 	 ;; Bindings for basic movements
  ;; 	 "SPC lsn" 'paredit-forward-slurp-sexp
  ;; 	 "SPC lsN" 'paredit-forward-barf-sexp
  ;; 	 "SPC lsh" 'paredit-backward-slurp-sexp
  ;; 	 "SPC lsh" 'paredit-backward-barf-sexp
  ;; 	 "M-n" 'paredit-forward-slurp-sexp
  ;; 	 "M-h" 'paredit-forward-barf-sexp
  ;; 	 "g M-h" 'paredit-backward-slurp-sexp
  ;; 	 "g M-H" 'paredit-backward-barf-sexp
  ;; 	 ;; Needed because we use 'u' instead of 'c'
  ;; 	 "c" nil
  ;; 	 "C" nil
  ;; 	 "u" 'evil-paredit-change
  ;; 	 "U" 'evil-paredit-change-line
  ;; 	 "d" 'evil-paredit-delete
  ;; 	 "D" 'evil-paredit-delete-line
  ;; 	 "SPC lsu" 'evil-change
  ;; 	 "SPC lsU" 'evil-change-line
  ;; 	 "SPC lsd" 'evil-delete
  ;; 	 "SPC lsD" 'evil-delete-line)

  ;; 	(general-define-key
  ;; 	 :keymaps 'evil-paredit-mode-map
  ;; 	 :states 'visual
  ;; 	 ;; Surrounding region
  ;; 	 "SPC ls(" 'paredit-wrap-round
  ;; 	 "SPC ls)" 'paredit-wrap-round
  ;; 	 "SPC ls]" 'paredit-wrap-square
  ;; 	 "SPC ls[" 'paredit-wrap-square
  ;; 	 "SPC ls{" 'paredit-wrap-curly
  ;; 	 "SPC ls}" 'paredit-wrap-curly
  ;; 	 "SPC ls>" 'paredit-wrap-angled
  ;; 	 "SPC ls<" 'paredit-wrap-angled))

  (use-package expand-region
	:ensure t
	:commands (er/expand-region)
	:init
	(general-define-key
	 :states '(visual)
	 "r" 'er/expand-region))

  (use-package evil-nerd-commenter
  	:ensure t
	:commands (evilnc-comment-operator)
  	:init
  	(general-define-key
  	 :states '(normal visual)
  	 "g/" 'evilnc-comment-operator))

  ;; As of June 14, evil-commentary is a bit buggy when the whitespace mode is on.
  ;; (use-package evil-commentary
  ;; 	:ensure t
  ;; 	:commands (evil-commentary)
  ;; 	:init
  ;; 	(general-define-key
  ;; 	 :states '(normal visual)
  ;; 	 "g/" 'evil-commentary))

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


  ;; (use-package aggressive-indent
  ;;   :hook ((prog-mode . aggressive-indent-mode)
  ;; 		 (text-mode . aggressive-indent-mode)))

  ;; (use-package fold-this
  ;;   :commands 'fold-active-region-all
  ;;   :init
  ;;   (general-define-key
  ;;    :states 'visual
  ;;    "." #'fold-active-region-all))

  ;; (use-package evil-numbers
  ;;   :commands (evil-numbers/dec-at-pt evil-numbers/inc-at-pt)
  ;;   :init
  ;;   (general-define-key
  ;;    :states '(normal visual)
  ;;    "g-" 'evil-numbers/dec-at-pt
  ;;    "g=" 'evil-numbers/inc-at-pt
  ;;    "g+" 'evil-numbers/inc-at-pt)
  )

