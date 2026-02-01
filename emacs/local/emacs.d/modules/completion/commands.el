;;; package -- My popup menus -*- lexical-binding: t; -*-

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
;; My command popup menus settings

;;; Code:

(use-package which-key
  :config
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "<f1> B" 'which-key-show-top-level ;; current active keybindings
   "<f1> \\" 'which-key-show-keymap) ;; any keymaps

  (general-define-key
   :keymaps 'which-key-C-h-map
   "<escape>" 'which-key-abort
   "d" 'which-key-toggle-docstrings
   "n" 'which-key-show-next-page-cycle
   "h" 'which-key-show-previous-page-cycle
   "z" 'which-key-undo-key)

  (setq which-key-allow-evil-operators t
        which-key-idle-delay 1 ;; for the first menu
        which-key-idle-secondary-delay 0 ;; for other secondary menus
        which-key-popup-type 'side-window
        which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-side-window-max-height (* max-mini-window-height 3))

  (which-key-mode 1))

(use-package embark
  :ensure t
  :commands (embark-act embark-bindings)
  :init
  (general-define-key
   :keymaps 'override
   :states 'normal
   "C-l" 'embark-act
   "M-l" 'embark-dwim
   "<f1> i" 'view-lossage
   "<f1> l" 'embark-bindings)

  (general-define-key
   :keymaps 'override
   "C-l" 'embark-act
   "M-l" 'embark-dwim
   "<f1> i" 'view-lossage
   "<f1> l" 'embark-bindings)
  :config
  (add-to-list 'display-buffer-alist
               '("\\\\*Embark"  ; Match any buffer starting with *Embark
                 (display-buffer-at-bottom)
                 (window-height . fit-window-to-buffer)
                 (pop-up-frames . nil)))

  (setq prefix-help-command #'embark-prefix-help-command)

  (setq embark-indicators
        '(embark-mixed-indicator))

;; (setq embark-indicators
;;       '(embark-minimal-indicator  ; default is embark-mixed-indicator
;;         embark-highlight-indicator
;;         embark-isearch-highlight-indicator))

  ;; (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  ;; (vertico-multiform-mode)

  )

(use-package embark-consult
  :ensure t
  :demand t)

(provide 'do-commands)

;;; commands.el ends here
