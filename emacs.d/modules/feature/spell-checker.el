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

(use-package flyspell
  :ensure t
  :hook ((text-mode . flyspell-mode)
		 (prog-mode . flyspell-prog-mode))
  :config
  (general-define-key
   :keymaps 'flyspell-mode-map
   "M-]" 'evil-next-flyspell-error
   "M-[" 'evil-prev-flyspell-error)

  (general-define-key
   :keymaps 'flyspell-mouse-map
   "C-<return>" 'ispell-word
   "M-<return>" 'ispell-word)

  (setq ispell-personal-dictionary "~/notes/personal.aspell.en.pws"
		flyspell-issue-message-flag nil
		flyspell-delay 5
		ispell-help-in-bufferp nil)

  (add-hook 'text-mode-hook 'flyspell-buffer)
  (add-hook 'prog-mode-hook 'flyspell-buffer))

(use-package flyspell-correct
  :ensure t
  :commands 'flyspell-correct-wrapper
  :init
  (general-define-key
   :keymaps 'flyspell-mouse-map
   "C-<return>" 'flyspell-correct-at-point))


(provide 'do-spell-checker)
