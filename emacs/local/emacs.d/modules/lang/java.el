;;; package -- my java configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Java configurations

;;; Code:

(use-package eglot-java
  :ensure t
  :hook
  (java-mode . eglot-java-mode)
  :config
  ;; Disable annoying symbol hover suggestions
  (add-to-list 'eglot-stay-out-of 'eldoc)
  (add-hook 'java-mode-hook
            #'(lambda ()
                (setq-local eldoc-idle-delay 0.5)
                ;; Tell the Java server to use up to 4GB of RAM
                ;; and use the G1 Garbage Collector for speed.
                (setq-local eglot-workspace-configuration
                            '(:java (:jdt (:ls (:vmargs "-Xmx8G" "-XX:+UseG1GC")))))))

  (add-hook 'before-save-hook
            #'(lambda ()
                (when (derived-mode-p 'java-mode)
                  (eglot-format (point-min) (point-max))))))

(add-hook 'java-mode-hook
          #'(lambda ()
              (require 'evil)
              (setq tab-width 4
                    evil-shift-width 4
                    indent-tabs-mode t)))

(provide 'do-java)

;;; java.el ends here
