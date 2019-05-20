;;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :after (evil)
  :functions evil-set-initial-state
  :commands (magit-status)
  :init
  (general-define-key
   :prefix "SPC g"
   :keymaps 'override
   :states 'normal
   ;; make a prefix-command and add description
   "" '(:ignore t :which-key "go")
   "g" #'magit-status)
  :config

  ;; Make sure all environment variables are set
  (eval-and-compile
  (unless (fboundp 'exec-path-from-shell)
	(require 'exec-path-from-shell)))

  (general-define-key
   :keymaps 'magit-status-mode-map
   "u" 'magit-unstage
   "s" 'magit-stage
   [return] 'magit-visit-thing
   [tab] 'magit-section-toggle)

  (general-define-key
   :keymaps 'magit-status-mode-map
   :prefix "l"
   :states 'normal
   "c" 'magit-commit
   "s" 'magit-stage
   "S" 'magit-stage-modified
   "u" 'magit-unstage
   "U" 'magit-unstage-all
   "k" 'magit-discard
   "p" #'magit-push-other
   "P" #'magit-pull-branch
   "f" #'magit-fetch-other
   "F" #'magit-fetch-all
   "b" #'magit-branch-and-checkout
   "m" #'magit-merge-into
   "SPC v" '(:ignore t :which-key "view")
   "vb" #'magit-blame
   "vl" #'magit-log-other
   "vd" #'magit-diff)

  (evil-set-initial-state 'magit-popup-mode 'emacs)

  (set-face-attribute 'magit-section-heading nil
					  :foreground chocolate-theme-element )
  (set-face-attribute 'magit-section-highlight nil :inherit 'region :background nil)
  (set-face-attribute 'magit-diff-hunk-heading
					  nil :inherit 'lazy-highlight
					  :background nil
					  :foreground nil)
  (set-face-attribute 'magit-diff-hunk-heading-highlight
					  nil :inherit 'lazy-highlight
					  :background nil
					  :foreground nil)
  (set-face-attribute 'magit-diff-context
					  nil
					  :background nil
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'magit-diff-context-highlight
					  nil
					  :background nil
					  :foreground chocolate-theme-shadow+3)
  (set-face-attribute 'magit-blame-heading
					  nil :inherit 'secondary-selection
					  :background chocolate-theme-shadow+2
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-blame-highlight
					  nil :inherit 'lazy-highlight
					  :background chocolate-theme-shadow+2
					  :foreground chocolate-theme-highlight+2)
  (set-face-attribute 'magit-head nil :foreground chocolate-theme-element+4 )
  (set-face-attribute 'magit-branch-remote nil
					  :foreground chocolate-theme-highlight+2 )
  (set-face-attribute 'magit-branch-remote-head nil
					  :foreground chocolate-theme-highlight+2 )
  ;; (set-face-attribute 'magit-diffstat-removed nil
  ;;:foreground chocolate-theme-highlight)
  ;; (set-face-attribute 'magit-diffstat-removed nil
  ;; :foreground chocolate-theme-highlight)
  ;; (set-face-attribute 'magit-diffstat-added nil :foreground "#8ae234")
  )

(use-package diff-hl
  :ensure t
  :defines (vc-git-diff-switches)
  :hook ((prog-mode . diff-hl-mode)
		 (text-mode . diff-hl-mode)
		 (dired-mode . diff-hl-dir-mode)
		 ;; (find-file . diff-hl-flydiff-mode)
		 )
  :config
  (setq vc-git-diff-switches '("--histogram"))

  (general-define-key
   :kemaps 'diff-hl-mode-map
   :states '(normal visual)
   "T" 'diff-hl-next-hunk
   "C" 'diff-hl-previous-hunk)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;(use-package orgit
;;  :after (org))
