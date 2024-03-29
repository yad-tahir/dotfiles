;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020

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

(defcustom chocolate-theme-bg "#000000"
  "Palette - default background"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-white "#e6beae"
  "Palette - default foreground"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-white+1 "#bd9c8f"
  "Palette - foreground class 1"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-white+2 "#eac9bc"
  "Palette - foreground class 2"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-white+3 "#efd5cb"
  "Palette - foreground class 4"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-shadow "#403333"
  "Palette - default shadow"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-shadow+1 "#392626"
  "Palette - shadow class 1"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-shadow+2 "#4d2900"
  "Palette - shadow class 3"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-shadow+3 "#654d4d"
  "Palette - shadow class 4"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-highlight "#ff00ff"
  "Palette - default highlight"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-highlight+1 "#ff6347"
  "Palette - highlight class 1"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-highlight+2 "#ffa500"
  "Palette - highlight class 2"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-highlight+3 "#ffc05c"
  "Palette - highlight class 3"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-element "#a7e23e"
  "Palette - default element"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+1 "#f0f0a0"
  "Palette - element class 1"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+2 "#c0ff3e"
  "Palette - element class 2"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+3 "#e0d0c1"
  "Palette - element class 3"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-element+4 "#da70d6"
  "Palette - element class 9"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+5 "#7818b4"
  "Palette - element class 10"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+6 "#fa8072"
  "Palette - element class 11"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+7 "#bb6083"
  "Palette - element class 8"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-element+8 "#00ffff"
  "Palette - element class 4"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+9 "#6fd8ff"
  "Palette - element class 5"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+10 "#3ee2b4"
  "Palette - element class 6"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+11 "#bef6d4"
  "Palette - element class 7"
  :type 'string :group 'chocolate-theme)

(deftheme chocolate
  "Chocolate - A delicious brown theme!")

;; NOTE: This files only contains the faces of Emacs built-in. The faces of
;; third-party packages should be handled in their corresponding files.

(custom-theme-set-faces
 'chocolate

 ;; basic
 `(cursor ((t (:background ,chocolate-theme-white
						   :foreground ,chocolate-theme-white))))

 ;; Avoid setting the height property as it can cause font-scaling, hence adding
 ;; delay to startup time. Set the font size in the .Xresource file instead.
 `(default ((t (:foreground ,chocolate-theme-white
							:background ,chocolate-theme-bg
							:weight normal :slant normal
							:underline nil :overline nil
							:strike-through nil :box nil
							:inverse-video nil :stipple nil
							:inherit nil))))
 `(warning ((t (:background nil :foreground ,chocolate-theme-highlight
							:weight bold))))
 `(bold ((t (:background nil :foreground ,chocolate-theme-highlight+3
						 :weight bold))))
 `(success ((t (:foreground ,chocolate-theme-element+9))))
 `(error ((t (:background nil :foreground ,chocolate-theme-highlight
						  :weight bold))))
 `(highlight ((t (:foreground ,chocolate-theme-element+4 :underline t))))
 `(region ((t (:background ,chocolate-theme-shadow+1
						   :foreground ,chocolate-theme-highlight+2))))
 `(secondary-selection ((t (:background ,chocolate-theme-shadow))))
 `(lazy-highlight ((t (:inherit secondary-selection
								:background ,chocolate-theme-shadow
								:foreground ,chocolate-theme-white+3))))
 `(header-line ((t (:inherit lazy-highlight
							 :background ,chocolate-theme-bg))))
 `(link ((t (:inherit highlight :underline nil))))
 `(link-visited ((t (:foreground ,chocolate-theme-element+8))))
 `(button ((t (:inherit (link) :underline t))))
 `(match ((t (:foreground ,chocolate-theme-bg
						  :background ,chocolate-theme-highlight))))
 `(minibuffer-prompt ((t (:foreground ,chocolate-theme-highlight+1))))
 `(escape-glyph ((t (:inherit shadow)))) ;;display non-graphic characters
 `(next-error ((t (:inherit (region)))))
 `(query-replace ((t (:inherit (isearch)))))
 `(shadow ((t (:foreground ,chocolate-theme-shadow+3))))
 `(fringe ((t (:foreground ,chocolate-theme-bg))))
 `(variable-pitch ((t (:family "Sans Serif"))))
 `(tooltip ((t (:inherit variable-pitch
						 :background ,chocolate-theme-white+1
						 :foreground ,chocolate-theme-bg))))
 `(fixed-pitch ((t (:family "Monospace"))))
 `(hl-line ((t (:inherit nil :background ,chocolate-theme-shadow))))
 `(compilation-mode-line-exit ((t (:foreground nil))))
 `(compilation-mode-line-error ((t (:foreground nil))))
 `(completions-annotations ((t (:foreground ,chocolate-theme-shadow+3))))
 `(completions-common-part ((t (:foreground ,chocolate-theme-shadow+3))))
 `(widget-documentation ((t (:foreground ,chocolate-theme-white+1))))
 `(widget-field ((t (:background ,chocolate-theme-shadow
								 :foreground ,chocolate-theme-white+3))))
 `(widget-button ((t (:inherit button))))
 `(widget-single-line-field ((t (:inherit widget-field))))

 ;; font-lock
 `(font-lock-builtin-face ((t (:foreground ,chocolate-theme-element+8))))
 `(font-lock-comment-delimiter-face ((t (:inherit
										 (font-lock-comment-face)))))
 `(font-lock-comment-face ((t (:inherit shadow :foreground nil))))
 `(font-lock-warning-face ((t (:inherit warning :foreground nil :weight normal))))
 `(font-lock-function-name-face ((t (:foreground ,chocolate-theme-element+2))))
 `(font-lock-variable-name-face ((t (:foreground ,chocolate-theme-white))))
 `(font-lock-constant-face ((t (:foreground ,chocolate-theme-element+9))))
 `(font-lock-keyword-face ((t (:foreground ,chocolate-theme-element+4))))
 `(font-lock-type-face ((t (:foreground ,chocolate-theme-highlight+3))))
 `(font-lock-preprocessor-face ((t (:foreground ,chocolate-theme-element+8))))
 `(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-string-face))))
 `(font-lock-regexp-grouping-construct ((t (:foreground
											,chocolate-theme-highlight+1))))
 `(font-lock-string-face ((t (:foreground ,chocolate-theme-highlight+3))))
 `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground nil))))
 `(show-paren-match ((t (:background ,chocolate-theme-element+5))))
 `(show-paren-mismatch ((t (:background ,chocolate-theme-highlight))))

 `(hi-blue ((t (:foreground ,chocolate-theme-bg
							:background ,chocolate-theme-element+8))))
 `(hi-blue-b ((t (:foreground ,chocolate-theme-bg
							  :background ,chocolate-theme-element+11))))
 `(hi-green ((t (:foreground ,chocolate-theme-bg
							 :background ,chocolate-theme-element))))
 `(hi-green-b ((t (:foreground ,chocolate-theme-bg
							   :background ,chocolate-theme-element))))
 `(hi-pink ((t (:foreground ,chocolate-theme-bg
							:background ,chocolate-theme-element+9))))
 `(hi-red-b ((t (:foreground ,chocolate-theme-bg
							 :background ,chocolate-theme-highlight+1))))
 `(hi-yellow ((t (:foreground ,chocolate-theme-bg
							  :background ,chocolate-theme-highlight+1))))

 ;; mode-line
 `(mode-line ((t (:background ,chocolate-theme-bg
							  :foreground ,chocolate-theme-white))))
 `(mode-line-buffer-id ((t (:weight bold))))
 `(mode-line-emphasis ((t (:foreground ,chocolate-theme-highlight+1
									   :weight bold))))
 `(mode-line-highlight ((t (:inherit (highlight)))))
 `(mode-line-inactive ((t (:background ,chocolate-theme-bg
									   :foreground ,chocolate-theme-shadow+3
									   :weight light :inherit mode-line))))

 ;; isearch
 `(isearch ((t (:inherit lazy-highlight :foreground ,chocolate-theme-highlight))))
 `(isearch-fail ((t (:background ,chocolate-theme-highlight))))

 ;; diff
 `(diff-header ((t (:background ,chocolate-theme-shadow))))
 `(diff-file-header ((t (:background nil :inherit diff-header))))
 `(diff-index ((t (:background ,chocolate-theme-shadow+1 :inherit diff-header))))
 `(diff-context ((t (:foreground ,chocolate-theme-shadow+3))))
 `(diff-added ((t (:foreground ,chocolate-theme-element))))
 `(diff-changed ((t (:foreground ,chocolate-theme-highlight+2))))
 `(diff-removed ((t (:foreground ,chocolate-theme-highlight))))
 `(diff-refine-added ((t (:background ,chocolate-theme-element
									  :foreground ,chocolate-theme-bg))))
 `(diff-refine-changed ((t (:background ,chocolate-theme-highlight+2
										:foreground ,chocolate-theme-bg))))
 `(diff-refine-removed ((t (:background ,chocolate-theme-highlight
										:foreground ,chocolate-theme-bg))))

 `(ediff-even-diff-A ((t (:inherit diff-index))))
 `(ediff-even-diff-B ((t (:inherit diff-index))))
 `(ediff-even-diff-C ((t (:inherit diff-index))))
 `(ediff-even-diff-Ancestor ((t (:inherit diff-index
										  :background ,chocolate-theme-shadow+1))))
 `(ediff-odd-diff-A ((t (:inherit diff-index))))
 `(ediff-odd-diff-B ((t (:inherit diff-index))))
 `(ediff-odd-diff-C ((t (:inherit diff-index))))
 `(ediff-odd-diff-Ancestor ((t (:inherit diff-index
										 :background ,chocolate-theme-shadow+1))))
 `(ediff-fine-diff-A ((t (:inherit highlight :background nil))))
 `(ediff-fine-diff-B ((t (:inherit highlight :background nil))))
 `(ediff-fine-diff-C ((t (:inherit highlight :background nil))))
 `(ediff-fine-diff-Ancestor ((t (:inherit isearch :background nil))))
 `(ediff-current-diff-A ((t (:inherit diff-index :background nil))))
 `(ediff-current-diff-B ((t (:inherit diff-index :background nil))))
 `(ediff-current-diff-C ((t (:inherit diff-index :background nil))))
 `(ediff-current-diff-Ancestor ((t (:inherit diff-index :background nil))))

 `(smerge-refined-added ((t (:inherit diff-added))))
 `(smerge-refined-removed ((t (:inherit diff-removed))))
 `(smerge-refined-changed ((t (:inherit diff-changed))))
 `(smerge-base ((t (:background ,chocolate-theme-white+1))))
 `(smerge-markers ((t (:inherit lazy-highlight))))


 ;; dired-mode
 ;; `(dired-directory ((t (:inherit font-lock-constant-face))))
 ;; `(dired-flagged ((t (:inherit (diff-hl-delete)))))
 ;; `(dired-symlink ((t (:inherit link-visited))))

 ;; whitespace
 `(whitespace-trailing ((t (:background nil
										:foreground ,chocolate-theme-highlight+1))))
 `(whitespace-empty ((t (:foreground ,chocolate-theme-shadow+1))))
 `(whitespace-hspace ((t (:inherit (whitespace-empty)))))
 `(whitespace-indentation ((t (:inherit (whitespace-empty)))))
 `(whitespace-line ((t (:inherit (whitespace-trailing)))))
 `(whitespace-newline ((t (:inherit (whitespace-empty)))))
 `(whitespace-space ((t (:inherit (whitespace-empty)))))
 `(whitespace-space-after-tab ((t (:inherit (whitespace-empty)))))
 `(whitespace-space-before-tab ((t (:inherit (whitespace-empty)))))
 `(whitespace-tab ((t (:inherit (whitespace-empty))))))

(provide-theme 'chocolate)

(provide 'chocolate-theme)

;;; chocolate.el ends here
