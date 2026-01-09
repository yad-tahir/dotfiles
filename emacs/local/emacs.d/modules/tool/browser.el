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


(use-package eww
  :defer t
  :commands (eww-browse-url)
  :config
  (general-define-key
   :states '(normal visual)
   :keymaps 'eww-mode-map
   "lo" 'eww-browse-with-external-browser
   "ly" 'eww-copy-page-url
   "<f5>" 'eww-reload)

  (general-define-key
   :keymaps 'eww-link-keymap
   "w" 'nil
   "u" 'nil
   "r" 'nil)

  (general-define-key
   :keymaps 'shr-map
   "w" 'nil
   "u" 'nil
   "r" 'nil)

  (set-face-attribute 'eww-valid-certificate nil
					  :inherit 'diff-added
					  :background 'nil
					  :foreground 'nil
					  :box 'nil)
  (set-face-attribute 'eww-invalid-certificate nil
					  :inherit 'diff-refine-removed
					  :background 'nil
					  :foreground 'nil
					  :box 'nil)
  (set-face-attribute 'eww-form-submit nil
					  :inherit 'widget-button
					  :background 'nil
					  :foreground 'nil
					  :box 'nil)
  (set-face-attribute 'eww-form-checkbox nil
					  :inherit 'widget-button
					  :background 'nil
					  :foreground 'nil
					  :box 'nil)
  (set-face-attribute 'eww-form-select nil
					  :inherit 'widget-button
					  :background 'nil
					  :foreground 'nil
					  :box 'nil)
  (set-face-attribute 'eww-form-file nil
					  :inherit 'widget-button
					  :background 'nil
					  :foreground 'nil
					  :box 'nil)
  (set-face-attribute 'eww-form-text nil
					  :inherit 'widget-single-line-field
					  :background 'nil
					  :foreground 'nil
					  :box 'nil)
  (set-face-attribute 'eww-form-textarea nil
					  :inherit 'widget-field
					  :background 'nil
					  :foreground 'nil
					  :box 'nil))


(provide 'do-browser)
