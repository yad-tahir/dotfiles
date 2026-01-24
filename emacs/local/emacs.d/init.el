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

;; (toggle-debug-on-error)

(require 'do-core (concat user-emacs-directory "modules/core/core"))

(do-modules-load "core/evil"

                 "ui/general"

                 "completion/commands"
                 "completion/company"
                 "completion/consult"
                 "completion/ivy"
                 "completion/snippets"

                 "feature/diff"
                 "feature/file-manager"
                 "feature/macros"
                 "feature/motions"
                 "feature/operators"
                 "feature/project"
                 "feature/spell-checker"
                 "feature/text-adjust"
                 "feature/tramp"

                 "lang/elisp"
                 "lang/org"
                 "lang/prog"

                 "tool/auth"
                 "tool/git"
                 "tool/license"
                 "tool/man"
                 "tool/music"
                 "tool/notebook"
                 "tool/scratch"
                 "tool/search"
                 "tool/shell"

                 "ui/bookmarks"
                 "ui/buffers"
                 "ui/line-numbering"
                 "ui/status-bar"
                 "ui/text-visual"
                 "ui/theme"
                 "ui/windows")
