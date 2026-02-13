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


(use-package projectile
  :disabled t
  :ensure t
  :commands (magit projectile-mode
                   projectile-switch-project
                   projectile-find-file projectile-find-dir)
  :init
  (general-define-key
   :keymaps 'override
   :states '(normal visual)
   "SPC sp" 'projectile-find-file
   "SPC sP" 'projectile-find-dir
   "SPC pp" 'projectile-switch-project)

  :config
  ;; Basic settings
  (setq projectile-enable-caching nil ;; no caching please. It is too slow!
        projectile-indexing-method 'alien
        projectile-completion-system 'ivy
        projectile-auto-discover t
        projectile-project-search-path '(("~/git" . 1) "~/dotfiles"))

  (general-define-key
   :keymaps 'projectile-mode-map
   :states 'normal
   :prefix "SPC p"
   "" '(:ignore t :which-key "project")
   "g" '(:ignore t :which-key "go")
   "gf" 'projectile-find-file-other-window
   "gd" 'projectile-find-dir-other-window
   "gw" 'projectile-find-file-dwim-other-window
   "gp" 'projectile-find-file-in-known-projects
   "g@" 'projectile-find-tag
   "!" '(:ignore t :which-key "shell")
   "!e" 'projectile-run-eshell
   "!t" 'projectile-run-term
   "!s" 'projectile-run-shell
   "!~" 'projectile-run-shell-command-in-root
   "!!" 'projectile-run-async-shell-command-in-root
   "<left>" 'projectile-previous-project-buffer
   "<right>" 'projectile-next-project-buffer
   "q" 'projectile-kill-buffers
   "w" 'projectile-save-project-buffers
   "r" 'projectile-replace
   "l" 'projectile-recentf
   "p" 'projectile-switch-project
   "f" 'projectile-find-file
   "F" 'projectile-find-dir
   "@" 'projectile-regenerate-tags
   "u" 'projectile-run-project
   "U" 'projectile-compile-project
   "t" 'projectile-test-project
   "T" 'projectile-toggle-between-implementation-and-test
   "v" 'projectile-vc
   "V" 'projectile-browse-dirty-projects
   "c" 'projectile-cache-current-file
   "C" 'projectile-invalidate-cache)

  (projectile-mode 1))


(provide 'do-project)
