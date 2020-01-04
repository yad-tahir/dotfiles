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


(use-package ispell
  :defer t
  :config

  (general-define-key
   :states 'normal
   "lS" 'ispell-buffer)

  (general-define-key
   :states 'visual
   "lS" 'ispell-region)

  ;; Prog-mode overrides
  (general-define-key
   :keymaps 'prog-mode-map
   :states 'normal
   "lS" 'ispell-comments-and-strings)

  (setq ispell-personal-dictionary "~/notes/personal.aspell.en.pws"
		ispell-dictionary "en_US"
		ispell-help-in-bufferp nil)

  ;; Use aspell or hunspell if it is possible
  (cond
   ;; Try hunspell at first
   ((executable-find "hunspell")
	(setq ispell-program-name "hunspell"
		  ispell-really-hunspell t))

   ((executable-find "aspell")
	(setq ispell-program-name "aspell"
		  ispell-really-aspell t
		  ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))))

(use-package flyspell
  :ensure t
  :hook ((text-mode . flyspell-mode)
		 (prog-mode . flyspell-prog-mode))
  :config
  (general-define-key
   :keymaps 'flyspell-mode-map
   :states '(normal visual)
   "]y" 'evil-next-flyspell-error
   "[y" 'evil-prev-flyspell-error)

  (general-define-key
   :keymaps 'flyspell-mouse-map
   "ly" 'ispell-word)

  (general-define-key
   :states 'normal
   "ls" 'flyspell-buffer)

  (general-define-key
   :states 'visual
   "ls" 'flyspell-region)

  (setq flyspell-issue-message-flag nil
		flyspell-delay 5
		;; Switch to the large mode as much as possible to avoid UI glitching
		flyspell-large-region 25)

  (defun do--spell-checker-timer ()
	"Called on a regular basis to highlight typos in the current window."
	;; When the backend is a file
	(when (buffer-file-name)
	  (save-excursion
		(with-local-quit
		  (flyspell-region (window-start)
						   (window-end nil t))))))

  (run-with-idle-timer 5 t 'do--spell-checker-timer))

(use-package flyspell-correct
  :ensure t
  :commands 'flyspell-correct-wrapper
  :init
  (general-define-key
   :keymaps 'flyspell-mouse-map
   "lY" 'flyspell-correct-at-point))



(provide 'do-spell-checker)
