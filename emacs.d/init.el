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
	;; ;; "tool/pdf"
	"tool/calculator"
	;; ;; "tool/dashboard"
	)
(do-modules-load t)
;; (do-modules-bootstrap-load)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-format-latex-options
   (list :foreground chocolate-theme-white :background chocolate-theme-bg :scale 1.0 :html-foreground chocolate-theme-white :html-background "Transparent" :html-scale 1.0))
 '(package-selected-packages
   (quote
	(diff-hl magit exec-path-from-shell eterm-256color fish-completion define-word simple-mpc flycheck-clojure cider clojure-mode company-go go-dlv go-mode macrostep flycheck org-bullets vlf openwith dired-subtree dired-collapse dired-open s drag-stuff evil-lion evil-commentary expand-region smartparens evil-surround auto-yasnippet yasnippet which-key company-quickhelp company-statistics company-flx company wgrep counsel-projectile counsel projectile ivy highlight-parentheses rainbow-mode evil-anzu telephone-line evil general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term-color-black ((t (:foreground "#654d4d" :background nil))))
 '(term-color-blue ((t (:foreground "#6fd8ff"))))
 '(term-color-cyan ((t (:foreground "#6fd8ff"))))
 '(term-color-green ((t (:foreground "#a7e23e"))))
 '(term-color-magenta ((t (:foreground "#6fd8ff"))))
 '(term-color-red ((t (:foreground "#ff4422"))))
 '(term-color-white ((t (:foreground "#e6beae"))))
 '(term-color-yellow ((t (:foreground "#ffa500"))))
 '(term-default-bg-color ((t (:inherit term-color-black))))
 '(term-default-fg-color ((t (:inherit term-color-white)))))
