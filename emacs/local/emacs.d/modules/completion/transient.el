;;; package -- my transient-like settings -*- lexical-binding: t; -*-

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
;; Contains all the configurations related to transient

;;; Code:

(use-package casual-suite
  :ensure t
  :commands (casual-calc-tmenu
             casual-dired-tmenu casual-isearch-tmenu
             casual-ibuffer-tmenu casual-ibuffer-filter-tmenu
             casual-ibuffer-sortby-tmenu casual-info-tmenu
             casual-re-builder-tmenu casual-re-builder-tmenu
             casual-bookmarks-tmenu casual-agenda-tmenu
             casual-editkit-main-tmenu casual-avy-tmenu
             casual-symbol-overlay-tmenu)
  :init
  (general-define-key :keymaps 'calc-mode-map "C-l" 'casual-calc-tmenu)
  (general-define-key :keymaps 'dired-mode-map "C-l" 'casual-dired-tmenu)
  (general-define-key :keymaps 'isearch-mode-map "C-l" 'casual-isearch-tmenu)
  (general-define-key :keymaps 'ibuffer-mode-map "C-l" 'casual-ibuffer-tmenu)
  (general-define-key :keymaps 'ibuffer-mode-map "C-f" 'casual-ibuffer-filter-tmenu)
  (general-define-key :keymaps 'ibuffer-mode-map "C-s" 'casual-ibuffer-sortby-tmenu)
  (general-define-key :keymaps 'Info-mode-map "C-l" 'casual-info-tmenu)
  (general-define-key :keymaps 'reb-mode-map "C-l" 'casual-re-builder-tmenu)
  (general-define-key :keymaps 'reb-lisp-mode-map "C-l" 'casual-re-builder-tmenu)
  (general-define-key :keymaps 'bookmark-bmenu-mode-map "C-l" 'casual-bookmarks-tmenu)
  (general-define-key :keymaps 'org-agenda-mode-map "C-l" 'casual-agenda-tmenu)
  (general-define-key :keymaps 'symbol-overlay-map "C-l" 'casual-symbol-overlay-tmenu)
  (general-define-key "C-l" 'casual-editkit-main-tmenu)
  (general-define-key "M-g" 'casual-avy-tmenu))

(provide 'do-transient)
;;; transient.el ends here
