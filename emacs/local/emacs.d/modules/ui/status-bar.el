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

;;; Code:

;; Make the color of the mode line dynamic
(use-package evil-anzu
  :ensure t
  :after evil
  :config
  (global-anzu-mode 1))

(use-package telephone-line
  :ensure t
  :demand t
  :defines (chocolate-theme-bg
            chocolate-theme-white chocolate-theme-highlight
            chocolate-theme-element+4 flycheck-last-status-change
            flycheck-current-errors)
  :functions (telephone-line-mode
              anzu--update-mode-line flycheck-count-errors
              flycheck-list-errors)
  :config
  (setq telephone-line-faces
        '((evil . telephone-line-modal-face)
          (alert . (telephone-line-warning . telephone-line-warning))
          (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
          (nil . (mode-line . mode-line-inactive))))

  (defun do--status-bar-anzu-segment ()
    "A telephone line segment returns `Anzu segment."

    (require 'anzu)
    (setq anzu-cons-mode-line-p nil)
    (lambda (_)
      (when anzu--state
        (telephone-line-raw (concat (anzu--update-mode-line) " " )))))

  (defun do--status-bar-major-mode-segment ()
    "A telephone line segment returns a single space."

    (lambda (_)
      (replace-regexp-in-string "-mode$"
                                ""
                                (symbol-name major-mode))))

  (defun do--status-bar-space-segment ()
    "A telephone line segment returns a single space."

    (lambda (_) " "))

  (defun do--status-bar-macro-segment ()
    "A telephone line segment for macro recording"

    (lambda (_)
      (if defining-kbd-macro
          (if evil-this-macro
              (telephone-line-raw
               (format " REC:%s" (char-to-string evil-this-macro)) nil)
            (telephone-line-raw  " REC" nil))
        (telephone-line-raw nil nil))))
  (defun do--status-bar-register-segment ()
    "A telephone line segment for active register"

    (lambda (_)
      (if evil-this-register
          (telephone-line-raw
           (format " REG:%s" (char-to-string evil-this-register)) nil)
        (telephone-line-raw nil nil))))

  (defun do--status-bar-total-length-segment ()
    "A telephone line segment returns the total number of characters in the current
buffer."

    (lambda (_)
      (telephone-line-raw (format " %s " (point-max)) nil)))

  (defun do--status-bar-mode-line-segment ()
    "A telephone line segment returns the mode line of the current buffer."

    (lambda (_)
      (telephone-line-raw (concat
                           ;; Buffer name
                           (telephone-line-raw mode-line-buffer-identification t)
                           ;; Indicators for recursive edits
                           (let ((result " ")
                                 (i (recursion-depth)))
                             (while (> i 0)
                               (setq i (- i 1))
                               (setq result (concat result "+")))
                             result)))))

  (telephone-line-defsegment do--status-bar-flycheck-segment ()
    "Displays current checker state."
    (when (bound-and-true-p flycheck-mode)
      (ignore face)
      (let* ((text (pcase flycheck-last-status-change
                     ('finished (if flycheck-current-errors
                                    (let-alist (flycheck-count-errors flycheck-current-errors)
                                      (let ((error (or .error 0))
                                            (warning (or .warning 0))
                                            (info (or .info 0)))
                                        ;; Customize this format string to remove "Problems:"
                                        (propertize
                                         (format " ✘ %s ‼ %s i %s" error warning info)
                                         'face 'telephone-line-error)))
                                  (propertize " ✔" 'face 'telephone-line-unimportant))) ;; No errors
                     ('running (if (bound-and-true-p flycheck-process-source)
                                  (propertize " ⟳" 'face 'telephone-line-projectile)
                                  (propertize " ⟳" 'face 'telephone-line-projectile)))
                     ('no-checker  (propertize "-" 'face 'telephone-line-unimportant))
                     ('not-checked "=")
                     ('errored     (propertize "!" 'face 'telephone-line-error))
                     ('interrupted (propertize "." 'face 'telephone-line-error))
                     ('suspicious  "?"))))
        (propertize text
                    'help-echo (pcase flycheck-last-status-change
                                 ('finished "Display errors found by Flycheck")
                                 ('running "Running...")
                                 ('no-checker "No Checker")
                                 ('not-checked "Not Checked")
                                 ('errored "Error!")
                                 ('interrupted "Interrupted")
                                 ('suspicious "Suspicious?"))
                    'display '(raise 0.0)
                    'mouse-face '(:box 1)
                    'local-map (make-mode-line-mouse-map
                                'mouse-1 #'flycheck-list-errors)))))

  (defun do--status-bar-position-segment ()
    "Position segment imitating vim-airline's appearance."
    (lambda (_)
      (concat " %c " ;; column
              ;; vertical scrolling
              (format "%2d" (/ (line-number-at-pos)
                               0.01
                               (line-number-at-pos (point-max))))
              "%%")))

  (setq telephone-line-lhs '((alert . (do--status-bar-macro-segment
                                       do--status-bar-register-segment))
                             (evil . (telephone-line-evil-tag-segment))
                             (accent . (telephone-line-vc-segment
                                        telephone-line-erc-modified-channels-segment
                                        telephone-line-process-segment))
                             (nil . (do--status-bar-anzu-segment))
                             (nil . (do--status-bar-mode-line-segment)))

        telephone-line-rhs '((nil . (do--status-bar-flycheck-segment
                                     telephone-line-misc-info-segment))
                             (accent . (do--status-bar-major-mode-segment
                                        do--status-bar-space-segment
                                        telephone-line-filesize-segment
                                        do--status-bar-position-segment)))

        ;; remove the default location of anzu segment
        mode-line-modified ""
        telephone-line-primary-left-separator		'telephone-line-nil
        telephone-line-primary-right-separator		'telephone-line-nil
        telephone-line-secondary-left-separator         'telephone-line-nil
        telephone-line-secondary-right-separator	'telephone-line-nil
        telephone-line-evil-use-short-tag nil)

  (telephone-line-mode)

  (with-eval-after-load 'chocolate-theme
    (progn
      (defun do--status-bar-change-mode-line-color(&rest _)
        (let ((color (cond (buffer-read-only
                            `(,chocolate-theme-bg . ,chocolate-theme-element+4))
                           ((buffer-modified-p)
                            `(,chocolate-theme-bg . ,chocolate-theme-highlight))
                           (t
                            `(,chocolate-theme-bg . ,chocolate-theme-white)))))
          (set-face-background 'mode-line (car color))
          (set-face-foreground 'mode-line (cdr color))))
      (add-hook 'post-command-hook 'do--status-bar-change-mode-line-color)
      (add-hook 'windmove-do-window-select 'do--status-bar-change-mode-line-color)
      (add-hook 'find-file-hook 'do--status-bar-change-mode-line-color))))

(provide 'do-status-bar)
