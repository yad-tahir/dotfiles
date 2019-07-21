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

;;; Code:
(use-package latte
  :defer 5
  :load-path "~/.emacs.d/local-packages/latte"

  :hook ((find-file . latte-mode)
		 ;;Avoid using the 'after-change-major-mode' hook as it can cause
		 ;;problems for some modes, e.g. org-agenda
		 ;; (after-change-major-mode . latte-mode)
		 )

  :commands (latte-files latte-search
						 latte-insert-keyword latte-insert-org-tag
						 latte-grep-topic latte-new-entry
						 latte-grep-all)
  :init
  (setq latte-directory "~/notes/notebook"
		latte-scan-idle-delay 30
		latte-rehighlight-after-scan t
		latte-ignore-words '("attach"))

  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC n" '(:ignore t :which-key "notes")
   "SPC nf" 'latte-files
   "SPC ns" 'latte-search
   "SPC nk" 'latte-insert-keyword
   "SPC ng" 'latte-grep-topic
   "SPC nG" 'latte-grep-all
   "SPC nc" 'latte-new-entry
   "SPC sn" 'latte-search)

  (defun latte()
	(interactive)
	(do-make-frame "notebook")
	(latte-search))

  :config
  (general-define-key
   :states '(normal visual)
   "ln" 'latte-grep-topic
   "lN" 'latte-grep-all
   "<M-return>" 'latte-grep-topic
   "<M-S-return>" 'latte-grep-all)

  (with-eval-after-load 'org
	(general-define-key
	 :keymaps 'org-mode-map
	 :states '(normal visual)
	 "SPC l#" 'latte-insert-org-tag)))


(provide 'do-notebook)
