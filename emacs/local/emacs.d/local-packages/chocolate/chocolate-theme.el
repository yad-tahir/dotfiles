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

(defcustom chocolate-theme-shadow "#403333"
  "Palette - default shadow"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-shadow+1 "#392626"
  "Palette - shadow class 1"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-shadow+2 "#654d4d"
  "Palette - shadow class 4"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-highlight "#ffba00"
  "Palette - default highlight"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-highlight+1 "#ffc07c"
  "Palette - highlight class 1"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-highlight+2 "#ff77ff"
  "Palette - highlight class 2"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-element "#e0d0c1"
  "Palette - element class 0"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+1 "#c0d040"
  "Palette - element class 1"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+2 "#99ffaa"
  "Palette - element class 2"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-element+3 "#b170da"
  "Palette - element class 3"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+4 "#ff77ff"
  "Palette - element class 4"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+5 "#7818b4"
  "Palette - element class 5"
  :type 'string :group 'chocolate-theme)

(defcustom chocolate-theme-element+6 "#bef6d4"
  "Palette - element class 6"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+7 "#6fd8ff"
  "Palette - element class 7"
  :type 'string :group 'chocolate-theme)
(defcustom chocolate-theme-element+8 "#00ffff"
  "Palette - element class 8"
  :type 'string :group 'chocolate-theme)

(deftheme chocolate
  "Chocolate - A delicious brown theme!")

;; NOTE: This files only contains the faces of Emacs built-in. The faces of
;; third-party packages should be handled in their corresponding files.

(custom-theme-set-faces
 'chocolate

 ;; basic
 `(cursor ((t (:background ,chocolate-theme-white :foreground ,chocolate-theme-bg :inverse-video nil :inherit nil))))

 ;; Avoid setting the height property as it can cause font-scaling, hence adding
 ;; delay to startup time. Set the font size in the .Xresource file instead.
 `(default ((t (:foreground ,chocolate-theme-white
                            :background ,chocolate-theme-bg
                            :height unspecified
                            :weight normal :slant normal
                            :underline nil :overline nil
                            :strike-through nil :box nil
                            :inverse-video nil :stipple nil
                            :inherit nil))))
 `(warning ((t (:background unspecified :foreground ,chocolate-theme-highlight :weight bold))))
 `(bold ((t (:background unspecified :foreground ,chocolate-theme-highlight+1 :weight bold))))
 `(success ((t (:foreground ,chocolate-theme-element+7))))
 `(error ((t (:background unspecified :foreground ,chocolate-theme-highlight+2 :weight bold))))
 `(highlight ((t (:foreground ,chocolate-theme-highlight :underline t))))
 `(region ((t (:background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-highlight))))
 `(secondary-selection ((t (:background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-element+4))))
 `(lazy-highlight ((t (:inherit secondary-selection))))
 `(bookmark-face ((t (:foreground ,chocolate-theme-highlight+2  :distant-foreground ,chocolate-theme-highlight+2))))
 `(header-line ((t (:inherit lazy-highlight :background ,chocolate-theme-bg))))
 `(link ((t (:foreground ,chocolate-theme-highlight+1 :underline nil))))
 `(link-visited ((t (:foreground ,chocolate-theme-element+8))))
 `(button ((t (:inherit (link) :underline t))))
 `(info-node ((t (:foreground ,chocolate-theme-white))))
 `(match ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight))))
 `(minibuffer-prompt ((t (:foreground ,chocolate-theme-highlight+2))))
 `(escape-glyph ((t (:inherit shadow)))) ;;display non-graphic characters
 `(next-error ((t (:inherit (region)))))
 `(query-replace ((t (:inherit (isearch)))))
 `(shadow ((t (:foreground ,chocolate-theme-shadow+2))))
 `(fringe ((t (:foreground ,chocolate-theme-bg))))
 `(variable-pitch ((t (:family "Sans Serif"))))
 `(tooltip ((t (:inherit variable-pitch :background ,chocolate-theme-white+1 :foreground ,chocolate-theme-bg))))
 `(fixed-pitch ((t (:family "Monospace"))))
 `(hl-line ((t (:inherit nil :background "nil" :underline (:color ,chocolate-theme-shadow+2 :style dashes :position nil)))))
 `(compilation-mode-line-exit ((t (:foreground unspecified))))
 `(compilation-mode-line-error ((t (:foreground unspecified))))
 `(completions-annotations ((t (:foreground ,chocolate-theme-shadow+2))))
 `(completions-common-part ((t (:foreground ,chocolate-theme-shadow+2))))
 `(widget-documentation ((t (:foreground ,chocolate-theme-white+1))))
 `(widget-field ((t (:background ,chocolate-theme-shadow :foreground ,chocolate-theme-white+2))))
 `(widget-button ((t (:inherit button))))
 `(widget-single-line-field ((t (:inherit widget-field))))

 ;; font-lock
 `(font-lock-builtin-face ((t (:foreground ,chocolate-theme-element+1))))
 `(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 `(font-lock-comment-face ((t (:inherit shadow :foreground unspecified))))
 `(font-lock-warning-face ((t (:inherit warning :foreground unspecified :weight normal))))
 `(font-lock-function-name-face ((t (:foreground ,chocolate-theme-element+2 :weight	bold))))
 `(font-lock-variable-name-face ((t (:foreground ,chocolate-theme-white))))
 `(font-lock-constant-face ((t (:foreground ,chocolate-theme-element+7))))
 `(font-lock-keyword-face ((t (:foreground ,chocolate-theme-element+3))))
 `(font-lock-type-face ((t (:foreground ,chocolate-theme-highlight+1))))
 `(font-lock-preprocessor-face ((t (:foreground ,chocolate-theme-element+8))))
 `(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-string-face))))
 `(font-lock-regexp-grouping-construct ((t (:foreground ,chocolate-theme-highlight+2))))
 `(font-lock-string-face ((t (:foreground ,chocolate-theme-highlight+1))))
 `(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground unspecified))))
 `(which-func ((t (:inherit font-lock-string-face :foreground unspecified))))
 `(show-paren-match ((t (:background ,chocolate-theme-element+5 :foreground ,chocolate-theme-white))))
 `(show-paren-mismatch ((t (:background ,chocolate-theme-highlight+2 :foreground ,chocolate-theme-bg))))

 `(hi-blue ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-element+8))))
 `(hi-blue-b ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-element+8))))
 `(hi-green ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-element+2))))
 `(hi-green-b ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-element+1))))
 `(hi-pink ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-element+3))))
 `(hi-red-b ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight+2))))
 `(hi-yellow ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight))))

 ;; mode-line
 `(mode-line ((t (:background ,chocolate-theme-bg :foreground ,chocolate-theme-white
                              :box (:line-width (1 . 2) :style flat-button)))))
 `(mode-line-buffer-id ((t (:weight bold))))
 `(mode-line-emphasis ((t (:foreground ,chocolate-theme-highlight+2 :weight bold))))
 `(mode-line-highlight ((t (:inherit (highlight)))))
 `(mode-line-inactive ((t (:background ,chocolate-theme-bg :weight light :inherit shadow))))

 ;; isearch
 `(isearch ((t (:inherit lazy-highlight :foreground ,chocolate-theme-highlight))))
 `(isearch-fail ((t (:background ,chocolate-theme-highlight :foreground ,chocolate-theme-bg))))

 ;; customizes
 `(custom-button ((t (:background ,chocolate-theme-shadow+1))))
 `(icon-button ((t (:inherit (custom-button)))))
 `(custom-button-mouse ((t (:background ,chocolate-theme-shadow+1))))
 `(custom-button-pressed ((t (:background ,chocolate-theme-shadow))))
 `(custom-comment ((t (:background ,chocolate-theme-shadow+1))))
 `(custom-comment-tag ((t (:background ,chocolate-theme-shadow+1))))
 `(custom-changed ((t (:background ,chocolate-theme-element+4))))
 `(custom-modified ((t (:background ,chocolate-theme-element+5))))
 `(custom-link ((t (:foreground ,chocolate-theme-element+7))))
 `(tool-bar ((t (:background ,chocolate-theme-bg))))
 `(separator-line ((t (:background ,chocolate-theme-shadow :foreground ,chocolate-theme-bg))))

 ;; menus tabs
 `(menu ((t (:background ,chocolate-theme-shadow+1 :foreground "white" :inverse-video nil :bold 1))))
 `(tab-bar ((t (:background ,chocolate-theme-shadow+1))))
 `(tab-line ((t (:background ,chocolate-theme-shadow+1))))
 `(tab-bar-tab-inactive ((t (:background ,chocolate-theme-bg :foreground ,chocolate-theme-shadow ))))

 ;; Diff
 `(diff-header ((t (:background ,chocolate-theme-shadow))))
 `(diff-file-header ((t (:background unspecified :inherit diff-header))))
 `(diff-index ((t (:background ,chocolate-theme-shadow+1 :inherit diff-header))))
 `(diff-context ((t (:foreground ,chocolate-theme-shadow+2))))
 `(diff-added ((t (:inherit unspecified :foreground ,chocolate-theme-bg
                            :background ,chocolate-theme-element+2))))
 `(diff-changed ((t (:inherit unspecified :foreground ,chocolate-theme-bg
                              :background ,chocolate-theme-element+4))))
 `(diff-removed ((t (:inherit unspecified :foreground ,chocolate-theme-bg
                              :background ,chocolate-theme-highlight))))
 `(diff-refine-added ((t (:inherit unspecified :foreground ,chocolate-theme-bg :background ,chocolate-theme-element+2))))
 `(diff-refine-changed ((t (:inherit unspecified :foreground ,chocolate-theme-bg :background ,chocolate-theme-element+4))))
 `(diff-refine-removed ((t (:inherit unspecified :foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight))))
 `(diff-hl-insert ((t (:inherit unspecified :foreground ,chocolate-theme-bg :background ,chocolate-theme-element+2))))
 `(diff-hl-change ((t (:inherit unspecified :foreground ,chocolate-theme-bg :background ,chocolate-theme-element+4))))
 `(diff-hl-delete ((t (:inherit unspecified :foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight))))
 `(diff-hl-dired-insert ((t (:inherit diff-hl-insert :foreground ,chocolate-theme-element+2))))
 `(diff-hl-dired-change ((t (:inherit diff-hl-change :foreground ,chocolate-theme-element+4))))
 `(diff-hl-dired-delete ((t (:inherit diff-hl-delete :foreground ,chocolate-theme-highlight))))
 `(diff-hl-dired-ignored ((t (:background ,chocolate-theme-bg :foreground ,chocolate-theme-bg))))

 `(ediff-even-diff-A ((t (:inherit diff-index))))
 `(ediff-even-diff-B ((t (:inherit diff-index))))
 `(ediff-even-diff-C ((t (:inherit diff-index))))
 `(ediff-even-diff-Ancestor ((t (:inherit diff-index :background ,chocolate-theme-shadow+1))))
 `(ediff-odd-diff-A ((t (:inherit diff-index))))
 `(ediff-odd-diff-B ((t (:inherit diff-index))))
 `(ediff-odd-diff-C ((t (:inherit diff-index))))
 `(ediff-odd-diff-Ancestor ((t (:inherit diff-index :background ,chocolate-theme-shadow+1))))
 `(ediff-fine-diff-A ((t (:inherit highlight :background unspecified))))
 `(ediff-fine-diff-B ((t (:inherit highlight :background unspecified))))
 `(ediff-fine-diff-C ((t (:inherit highlight :background unspecified))))
 `(ediff-fine-diff-Ancestor ((t (:inherit isearch :background unspecified))))
 `(ediff-current-diff-A ((t (:inherit diff-index :background unspecified))))
 `(ediff-current-diff-B ((t (:inherit diff-index :background unspecified))))
 `(ediff-current-diff-C ((t (:inherit diff-index :background unspecified))))
 `(ediff-current-diff-Ancestor ((t (:inherit diff-index :background unspecified))))

 `(smerge-refined-added ((t (:inherit diff-added))))
 `(smerge-refined-removed ((t (:inherit diff-removed))))
 `(smerge-refined-changed ((t (:inherit diff-changed))))
 `(smerge-base ((t (:background ,chocolate-theme-white+1))))
 `(smerge-markers ((t (:inherit lazy-highlight))))


 ;; dired-mode
 `(dired-directory ((t (:inherit font-lock-builtin-face :weight bold))))
 `(dired-symlink ((t (:inherit font-lock-constant-face :weight bold))))
 `(dired-broken-symlink ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight+2))))
 `(dired-header ((t (:foreground ,chocolate-theme-element+3 :weight bold))))
 ;; `(dired-marked ((t (:weight normal :inherit warning))))

 ;; whitespace
 `(whitespace-trailing ((t (:background unspecified :foreground ,chocolate-theme-element+5))))
 `(trailing-whitespace ((t (:background ,chocolate-theme-highlight+2 :foreground ,chocolate-theme-bg))))
 `(whitespace-big-indent ((t (:background ,chocolate-theme-highlight+2 :foreground ,chocolate-theme-bg))))
 `(whitespace-empty ((t (:foreground ,chocolate-theme-shadow+1))))
 `(whitespace-hspace ((t (:inherit (whitespace-empty)))))
 `(whitespace-indentation ((t (:inherit (whitespace-empty)))))
 `(whitespace-line ((t (:inherit (whitespace-trailing)))))
 `(whitespace-newline ((t (:inherit (whitespace-empty)))))
 `(whitespace-space ((t (:inherit (whitespace-empty)))))
 `(whitespace-space-after-tab ((t (:inherit (whitespace-empty)))))
 `(whitespace-space-before-tab ((t (:inherit (whitespace-empty)))))
 `(whitespace-tab ((t (:inherit (whitespace-empty)))))

 `(line-number ((t (:foreground ,chocolate-theme-shadow+2 :weight normal))))
 `(line-number-current-line ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-shadow+2 :weight normal))))
 `(line-number-major-tick ((t (:foreground ,chocolate-theme-shadow+2 :weight bold :background unspecified))))
 `(line-number-minor-tick ((t (:foreground ,chocolate-theme-shadow+2 :weight normal :background unspecified))))
 `(fringe ((t (:background ,chocolate-theme-bg))))

 ;; Org Mode
 `(org-special-keyword ((t (:inherit ,font-lock-builtin-face))))
 `(org-level-1 ((t (:foreground ,chocolate-theme-element+3 :weight bold))))
 `(org-level-2 ((t (:foreground ,chocolate-theme-element+3 :weight normal))))
 `(org-level-3 ((t (:foreground ,chocolate-theme-white+2 :weight normal))))
 `(org-level-4 ((t (:foreground ,chocolate-theme-white+2 :weight normal))))
 `(org-level-5 ((t (:foreground ,chocolate-theme-white+2 :weight normal))))
 `(org-level-6 ((t (:foreground ,chocolate-theme-white+2 :weight normal))))
 `(org-level-7 ((t (:foreground ,chocolate-theme-white+2 :weight normal))))
 `(org-level-8 ((t (:foreground ,chocolate-theme-white+2 :weight normal))))
 `(org-block ((t (:foreground ,chocolate-theme-white+1))))
 `(org-todo ((t (:inherit font-lock-builtin-face))))
 `(org-done ((t (:foreground ,chocolate-theme-element))))
 `(org-headline-done ((t (:foreground ,chocolate-theme-element :weight normal))))
 `(org-table ((t (:foreground ,chocolate-theme-highlight+1))))
 `(org-priority ((t (:foreground unspecified))))
 `(org-document-info-keyword ((t (:inherit font-lock-builtin-face))))
 `(org-drawer ((t (:foreground ,chocolate-theme-shadow+2 :inherit font-lock-comment-face))))
 `(org-date ((t (:foreground ,chocolate-theme-highlight+1 :inherit font-lock-string-face))))
 `(org-scheduled-today ((t (:foreground ,chocolate-theme-white))))
 `(org-scheduled ((t (:foreground ,chocolate-theme-white))))
 `(org-scheduled-previously ((t (:foreground ,chocolate-theme-highlight+1))))
 `(org-clock-overlay ((t (:inherit org-column))))
 `(org-column ((t (:background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-white+2))))
 `(org-column-title ((t (:background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-highlight))))
 `(org-dispatcher-highlight ((t (:background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-white+2))))
 `(org-clock-overlay ((t (:background ,chocolate-theme-element+8))))
 `(org-mode-line-clock-overrun ((t (:foreground "white"))))

 `(org-habit-alert-face ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight))))
 `(org-habit-alert-future-face ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight))))
 `(org-habit-clear-face ((t (:foreground ,chocolate-theme-white :background ,chocolate-theme-shadow+2))))
 `(org-habit-clear-future-face ((t (:foreground unspecified :background unspecified :inherit org-habit-clear-face))))
 `(org-habit-ready-face ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-element+2))))
 `(org-habit-ready-future-face ((t (:foreground unspecified :background unspecified :inherit org-habit-ready-face))))
 `(org-habit-overdue-face ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight+2))))
 `(org-habit-overdue-future-face ((t (:foreground unspecified :background unspecified :inherit org-habit-overdue-face))))

 `(org-agenda-done ((t (:foreground ,chocolate-theme-white+1))))
 `(org-agenda-date-today ((t (:foreground ,chocolate-theme-white+2))))
 `(org-agenda-dimmed-todo-face ((t (:inherit font-lock-comment-face :foreground ,chocolate-theme-element+2))))
 `(org-agenda-date ((t (:foreground ,chocolate-theme-white+1))))
 `(org-time-grid ((t (:inherit shadow :foreground unspecified))))
 `(org-agenda-clocking ((t (:background ,chocolate-theme-shadow+1))))
 `(org-agenda-structure ((t (:foreground ,chocolate-theme-element+3))))

 `(org-roam-dim ((t (:inherit shadow))))
 `(org-roam-preview-heading ((t (:inherit org-column :foreground unspecified :background unspecified))))
 `(org-roam-preview-heading-highlight ((t (:inherit org-dispatcher-highlight :foreground unspecified :background unspecified))))

 ;; Evil
 `(evil-ex-substitute-replacement ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-highlight+1))))

 ;; Magit
 `(magit-section-heading ((t (:background unspecified :foreground ,chocolate-theme-element+4))))
 `(magit-section-highlight ((t (:inherit region :foreground unspecified :background ,chocolate-theme-shadow))))
 `(magit-diff-hunk-heading ((t (:inherit lazy-highlight :background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-highlight))))
 `(magit-diff-hunk-heading-highlight ((t (:inherit lazy-highlight :background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-highlight))))
 `(magit-diff-context ((t (:background ,chocolate-theme-bg :foreground ,chocolate-theme-shadow+2))))
 `(magit-diff-context-highlight ((t (:background ,chocolate-theme-bg :foreground ,chocolate-theme-shadow+2))))
 `(magit-blame-heading ((t (:inherit secondary-selection :background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-highlight))))
 `(magit-blame-highlight ((t (:inherit lazy-highlight :background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-highlight))))
 `(magit-head ((t (:foreground ,chocolate-theme-element+3))))
 `(magit-branch-remote ((t (:foreground ,chocolate-theme-highlight))))
 `(magit-branch-remote-head ((t (:foreground ,chocolate-theme-highlight))))
 `(magit-dimmed ((t (:inherit shadow :foreground ,chocolate-theme-shadow+2))))
 `(magit-hash ((t (:inherit shadow :foreground ,chocolate-theme-shadow+2))))
 `(magit-log-author ((t (:foreground ,chocolate-theme-element+7))))
 `(magit-process-ok ((t (:inherit font-lock-builtin-face))))
 `(magit-reflog-commit ((t (:inherit font-lock-builtin-face))))
 `(magit-reflog-merge ((t (:inherit font-lock-builtin-face))))
 `(magit-reflog-cherry-pick ((t (:inherit font-lock-builtin-face))))
 `(magit-reflog-checkout ((t (:inherit font-lock-constant-face))))
 `(magit-signature-good ((t (:inherit hi-green))))
 `(magit-signature-bad ((t (:inherit hi-red))))
 `(magit-signature-error ((t (:inherit hi-red))))
 `(magit-signature-expired ((t (:inherit hi-pink))))
 `(magit-signature-untrusted ((t (:inherit hi-pink))))
 `(magit-log-date ((t (:foreground ,chocolate-theme-white+2))))
 `(magit-log-graph ((t (:foreground ,chocolate-theme-white+2))))
 `(magit-diff-whitespace-warning ((t (:inherit trailing-whitespace :foreground unspecified :background unspecified))))

 ;;Third-party mode line
 `(anzu-mode-line ((t (:foreground ,chocolate-theme-element+2 :background ,chocolate-theme-bg))))
 `(anzu-mode-line-no-match ((t (:foreground ,chocolate-theme-highlight :background ,chocolate-theme-bg))))
 `(mode-line ((t (:box (:line-width (2 . 2) :style flat-button)))))
 `(telephone-line-evil-normal ((t (:background ,chocolate-theme-element+3 :foreground ,chocolate-theme-bg
                                               :inherit mode-line :weight normal))))
 `(telephone-line-evil-insert ((t (:background ,chocolate-theme-element+2 :foreground ,chocolate-theme-bg
                                               :inherit mode-line :weight normal))))
 `(telephone-line-evil-motion ((t (:background ,chocolate-theme-highlight+1 :foreground ,chocolate-theme-bg
                                               :inherit mode-line :weight normal))))
 `(telephone-line-evil-visual ((t (:background ,chocolate-theme-highlight :foreground ,chocolate-theme-bg
                                               :inherit mode-line :weight normal))))
 `(telephone-line-evil-operator ((t (:background ,chocolate-theme-element+6 :foreground ,chocolate-theme-bg
                                                 :inherit mode-line :weight normal))))
 `(telephone-line-evil-emacs ((t (:background ,chocolate-theme-highlight+2 :foreground ,chocolate-theme-bg
                                              :inherit mode-line :weight normal))))
 `(telephone-line-evil-replace ((t (:background ,chocolate-theme-element+4 :foreground ,chocolate-theme-bg
                                                :inherit mode-line :weight normal))))
 `(telephone-line-accent-active ((t (:background ,chocolate-theme-shadow+1 :foreground ,chocolate-theme-white+2
                                                 :inherit mode-line :weight normal))))
 `(telephone-line-accent-inactive ((t (:background ,chocolate-theme-bg :foreground ,chocolate-theme-shadow+2
                                                   :inherit mode-line :weight normal))))
 `(telephone-line-error ((t (:background unspecified :foreground ,chocolate-theme-highlight
                                         :inherit mode-line :weight normal))))
 `(telephone-line-unimportant ((t (:background unspecified :foreground ,chocolate-theme-shadow+2
                                               :inherit mode-line))))
 `(telephone-line-warning ((t (:background unspecified :foreground ,chocolate-theme-element+7
                                           :inherit mode-line :weight bold))))

 `(ivy-cursor ((t (:inherit cursor :foreground unspecified :background ,chocolate-theme-bg))))
 `(ivy-current-match ((t (:inherit nil :foreground ,chocolate-theme-bg :background ,chocolate-theme-element+2))))
 `(ivy-confirm-face ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-element+1))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-shadow+2))))
 `(ivy-minibuffer-match-face-2 ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-element+2 :weight bold))))
 `(ivy-minibuffer-match-face-3 ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-element+2 :weight bold))))
 `(ivy-minibuffer-match-face-4 ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-element+2 :weight bold))))
 `(ivy-virtual ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-shadow+2))))
 `(ivy-modified-buffer ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-highlight :weight normal))))
 `(ivy-modified-outside-buffer ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-highlight+2 :weight bold))))
 `(ivy-org ((t (:inherit nil :background unspecified :foreground ,chocolate-theme-white+1 :weight normal))))
 ;; Make ivy identical to some of the faces in dired mode. However,
 ;; we dont want to inherit from dired as this might trigger auto-loading
 `(ivy-subdir ((t (:inherit dired-directory))))
 `(ivy-remote ((t (:inherit font-lock-keyword-face :background unspecified :foreground unspecified))))

 `(flycheck-info ((t (:underline (:color ,chocolate-theme-shadow :style wave :position nil)))))
 `(flycheck-fringe-info ((t (:foreground ,chocolate-theme-element+3))))
 `(vundo-highlight ((t (:foreground ,chocolate-theme-highlight :weight bold))))
 `(vundo-last-saved ((t (:foreground ,chocolate-theme-element+2))))
 `(vundo-saved ((t (:foreground ,chocolate-theme-element+6))))
 `(vundo-stem ((t (:foreground ,chocolate-theme-element+3))))

 ;; Transient
 `(transient-key ((t (:foreground ,chocolate-theme-highlight))))
 `(transient-key-stack ((t (:inherit font-lock-builtin-face))))
 `(transient-key-stay ((t (:inherit font-lock-builtin-face))))
 `(transient-key-recurse ((t (:foreground ,chocolate-theme-element+2))))
 `(transient-key-return ((t (:foreground ,chocolate-theme-element+2))))
 `(transient-key-exit ((t (:foreground ,chocolate-theme-highlight))))
 `(transient-key-noop ((t (:foreground ,chocolate-theme-shadow))))
 `(transient-disabled-suffix ((t (:inherit diff-changed))))
 `(transient-disabled-suffix ((t (:inherit diff-added))))

 ;; Term
 `(term-color-black ((t (:foreground ,chocolate-theme-shadow+2 :background unspecified))))
 `(term-color-red ((t (:foreground ,chocolate-theme-highlight ))))
 `(term-color-green ((t (:foreground ,chocolate-theme-element ))))
 `(term-color-yellow ((t (:foreground ,chocolate-theme-highlight+1))))
 `(term-color-blue ((t (:foreground ,chocolate-theme-element+4))))
 `(term-color-magenta ((t (:foreground ,chocolate-theme-element+4))))
 `(term-color-cyan ((t (:foreground ,chocolate-theme-element+4))))
 `(term-color-white ((t (:foreground ,chocolate-theme-white))))
 `(term-default-fg-color ((t (:inherit term-color-white))))
 `(term-default-bg-color ((t (:inherit term-color-black))))

 ;; Company
 `(company-scrollbar-bg ((t (:background ,chocolate-theme-shadow+1))))
 `(company-scrollbar-fg ((t (:background ,chocolate-theme-shadow+2))))
 `(company-preview ((t (:inherit nil :foreground ,chocolate-theme-highlight+2 :background ,chocolate-theme-bg))))
 `(company-preview-common ((t (:inherit nil :foreground ,chocolate-theme-highlight+2))))
 `(company-preview-search ((t (:inherit nil :background ,chocolate-theme-highlight :foreground ,chocolate-theme-bg))))
 `(company-tooltip ((t (:foreground ,chocolate-theme-white :background ,chocolate-theme-shadow+1))))
 `(company-tooltip-scrollbar-thumb ((t (:foreground ,chocolate-theme-white :background ,chocolate-theme-shadow+2))))
 `(company-tooltip-scrollbar-track ((t (:foreground ,chocolate-theme-white :background ,chocolate-theme-shadow+1))))
 `(company-tooltip-selection ((t (:foreground ,chocolate-theme-element+2 :background ,chocolate-theme-shadow))))
 `(company-tooltip-annotation ((t (:foreground ,chocolate-theme-shadow+2))))
 `(company-tooltip-annotation-selection ((t (:foreground ,chocolate-theme-bg))))
 `(company-tooltip-common ((t (:foreground ,chocolate-theme-highlight :weight bold))))
 `(company-tooltip-common-selection ((t (:foreground ,chocolate-theme-element+2 :weight bold :background unspecified))))
 `(company-tooltip-search ((t (:foreground ,chocolate-theme-element+8 :background unspecified :underline nil :weight bold))))
 `(company-tooltip-search-selection ((t (:foreground ,chocolate-theme-bg :background ,chocolate-theme-element+8 :weight bold :underline nil)))))

;; Dynamic theme settings
(with-eval-after-load 'highlight-parentheses
  (setq highlight-parentheses-colors
        (list chocolate-theme-highlight chocolate-theme-element+4
              chocolate-theme-element+3 chocolate-theme-element+8
              chocolate-theme-highlight+2 chocolate-theme-element+1
              chocolate-theme-element+5 chocolate-theme-element+7
              chocolate-theme-highlight chocolate-theme-element+2
              chocolate-theme-highlight+1 chocolate-theme-element+6
              chocolate-theme-highlight chocolate-theme-element
              chocolate-theme-element+8 chocolate-theme-element+8)))

(provide-theme 'chocolate)

(provide 'chocolate-theme)

;;; chocolate.el ends here
