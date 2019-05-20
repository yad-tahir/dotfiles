(require 'core "~/.emacs.d/modules/core/core")

(do-modules
	"core/evil"
	"ui/general"
	"ui/theme" ;; don't put it on top; it increases the startup time somehow!
	"ui/status-bar" ;; slow
	"ui/windows"
	"ui/buffers"
	"ui/line-numbering"
	"ui/text-visual"

	"completion/ivy" ;; slow
	"completion/company"
	"completion/commands" ;; a bit slow
	"completion/snippets"

	"feature/scratches"
	"feature/text-adjust"
	"feature/spell-checker"
	"feature/string"
	"feature/lemon-bar"
	"feature/tramp"
	"feature/file-manager"

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
	"tool/music" ;; needs optimization
	"tool/english"
	"tool/shell" ;; cleaning
	"tool/git"
	;; "tool/pdf"
	"tool/calculator"
	;; "tool/dashboard"
	)
(do-modules-load t)
;; (do-modules-bootstrap-load)
