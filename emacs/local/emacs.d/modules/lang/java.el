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

;; (use-package eclim
;;   :hook ((java-mode-hook . eclim-mode))
;;   :ensure t
;;   :init
;;   (custom-set-variables '(eclim-eclipse-dirs '("/usr/lib/eclipse"))
;;						'(eclim-executable "/usr/lib/eclipse/eclim")
;;						'(eclimd-default-workspace "/home/yad/project/eclipse"))
;;   :config
;;   (setq eclim-autostart t))

;; (use-package company-emacs-eclim
;;	:after (:all company eclim)
;;	:ensure t
;;	:config
;;	(company-emacs-eclim-setup))


;; (use-package lsp-java
;;   :after (lsp-mode)
;;   :ensure t)


(provide 'do-java)
