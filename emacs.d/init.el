;; Copyright (C) 2019

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

(require 'core "~/.emacs.d/modules/core/core")

(do-modules
	"core/evil"
	"ui/general"
	"ui/theme" ;; don't put it on top; it increases the startup time somehow!
	"ui/status-bar"
	"ui/windows"
	"ui/buffers"
	"ui/line-numbering"
	"ui/text-visual"

	"completion/ivy" ;; slow
	"completion/company"
	"completion/commands" ;; slow
	"completion/snippets"

	"feature/scratches"
	"feature/text-adjust"
	"feature/spell-checker"
	"feature/string"
	"feature/lemon-bar"
	"feature/tramp"
	"feature/file-manager"
	"feature/diff"
	"feature/motion"
	"feature/macro"

	"lang/text"
	"lang/org" ;; slow
	"lang/prog"
	"lang/elisp"
	"lang/go"
	"lang/clojure"
	;; "lang/java"
	;; "lang/web"
	;; "lang/php"

	"tool/notebook"
	"tool/music" ;; needs improvements
	"tool/english"
	"tool/shell" ;; needs improvements
	"tool/git"
	"tool/calculator"
	"tool/license")

(do-modules-load t)
