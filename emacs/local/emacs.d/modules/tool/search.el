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

(eval-when-compile
  (use-package deadgrep
    :ensure t))

(use-package deadgrep
  :ensure t
  :commands (deadgrep do--minibuffer-to-deadgrep)
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC sG" 'deadgrep)

  (eval-after-load 'ivy
    (general-define-key
     :keymaps 'ivy-minibuffer-map
     "C-S-g" 'do--minibuffer-to-deadgrep))

  :config
  (general-define-key
   :keymaps 'deadgrep-mode-map
   :states '(normal visual)
   "<RET>" 'deadgrep-visit-result
   "<M-RET>" 'deadgrep-visit-result-other-window
   "TAB" 'deadgrep-toggle-file-results
   "N" 'deadgrep-forward
   "H" 'deadgrep-backward
   "SPC lq" 'deadgrep-kill-process
   "le" 'deadgrep-edit-mode
   "<f5>" 'deadgrep-restart)

  (general-define-key
   :keymaps 'deadgrep-edit-mode-map
   :states '(normal visual)
   "<RET>" 'deadgrep-visit-result)

  (setq deadgrep-max-buffers 1)

  (defun do--minibuffer-to-deadgrep ()
    "Use the current ivy input to launch a deadgrep search."
    (interactive)
    (let ((query ivy-text)) ;
      (ivy-quit-and-run
        (deadgrep query default-directory))))

  (add-hook 'deadgrep-mode-hook
            #'(lambda ()
                ;; Show hidden files by default
                (setq deadgrep--file-type (cons 'glob "*")))))

(use-package google-this
  :disabled t
  :ensure t
  :commands (google-this)
  :init
  (general-define-key
   :states '(normal visual)
   "SPC l <RET>" 'google-this)

  (general-define-key
   :states '(normal visual)
   "SPC li" 'define-wiki)

  (with-eval-after-load 'evil
    (evil-define-operator define-wiki (beginning end)
      "Get Wiki summary for the words between BEGINNING END."
      :move-point nil
      (require 'eww)
      (eww-browse-url
       (format
        "https://en.wikipedia.org/wiki/Special:Search/?search=%s&sourceid=emacs"
        (buffer-substring-no-properties beginning end))))))


(provide 'do-search)
