;;; -*- lexical-binding: t; -*-

(use-package dired
  :defer t
  :commands (do-file-manager dired)
  :init
  (general-define-key
   :keymaps 'override
   :prefix "SPC g"
   :states '(normal visual)
   "f" #'find-file
   "F" #'dired)
  (defun do-file-manager (&optional path)
	(interactive)
	(do-make-frame "file-manager")
	(if (null path)
		(dired default-directory)
	  (dired path)))
  :config

  (defun do-dired-find-marked-files-noframe ()
	(interactive)
	"Find all marked files in the dired buffer."
	(do--dired-simultaneous-find (dired-get-marked-files) nil))

  (defun do-dired-find-marked-files ()
	"Find all marked files in the dired buffer. Display each file in a separate frame."
	(interactive)
	(do--dired-simultaneous-find (dired-get-marked-files) t))

  (defun do--dired-simultaneous-find (file-list frame)
	"Find all marked files in the dired mode.
	If FRAME is t, create a separate frame for each file."
	(while (not (null file-list))
	  (let* ((file (car file-list)))
		(if frame
		  (progn
			(select-frame (make-frame))
			(find-file file))
		  (find-file-noselect file)))
	  (setq file-list (cdr file-list))))


  ;; Load wdired.el to customize some variables
  (cl-eval-when (compile)
	(require 'wdired))

  (setq wdired-allow-to-change-permissions t
		wdired-create-parent-directories t
		wdired-allow-to-redirect-links t
		dired-auto-revert-buffer t
		dired-no-confirm t)

  (general-define-key
   :keymaps 'wdired-mode-map
   :states 'normal
   "lq" 'wdired-finish-edit
   "w" 'evil-forward-word-begin
   "W" 'evil-forward-WORD-begin
   "b" 'evil-backward-word-begin
   "B" 'evil-backward-WORD-begin
   "l<escape>" 'wdired-abort-changes)

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   ;; general
   "t" 'dired-next-line
   "c" 'dired-previous-line
   ;; "<return>" 'dired-find-file
   "<return>" 'dired-find-alternate-file
   "M-<return>" '(lambda ()(interactive)
				   (do-make-frame)(dired-find-file-other-window))
   "{" 'dired-prev-dirline
   "}" 'dired-next-dirline
   "<" 'dired-prev-dirline
   ">" 'dired-next-dirline
   "DEL" 'dired-up-directory
   ";" 'nil
   "q" 'nil
   "l" 'nil
   "ls" 'dired-sort-toggle-or-edit
   ;; "f" 'counsel-dired-jump
   ;; "lf" 'counsel-dired-jump
   [remap next-line] 'dired-next-line
   [remap previous-line] 'dired-previous-line

   ;; operations
   ;; "x" 'dired-do-flagged-delete
   ;; "d" 'dired-flag-file-deletion
   "Y" 'dired-do-copy
   "D" 'dired-do-delete
   "R" 'dired-do-rename
   "M" 'dired-do-rename
   "F" 'find-file
   "+" 'dired-create-directory
   "O" 'dired-do-chown
   "L" 'dired-do-symlink
   "M-l" 'dired-do-hardlink
   "ld" 'dired-do-delete
   "ly" 'dired-do-copy
   "lr" 'dired-do-rename
   "lm" 'dired-do-rename
   "lf" 'find-file
   "l+" 'dired-create-directory
   "lw" 'dired-do-chown
   "ll" 'dired-do-symlink
   "lL" 'dired-do-hardlink
   "!" 'dired-do-shell-command
   "l!" 'dired-do-shell-command
   "&" 'dired-do-async-shell-command
   "l&" 'dired-do-async-shell-command
   ;; [tab] 'dired-summary
   "l=" 'dired-diff
   "lz" 'dired-do-compress
   "lZ" 'dired-do-compress-to
   "lt" 'dired-do-touch
   "lu" 'dired-downcase
   "lU" 'dired-upcase
   "z" 'dired-undo
   [remap undo] 'dired-undo
   [remap advertised-undo] 'dired-undo

   ;; mark
   "m" 'dired-mark
   "u" 'dired-unmark
   "U" 'dired-unmark-all-marks
   "*" nil
   "**" 'dired-mark-executables
   "*d" 'dired-mark-directories
   "*@" 'dired-mark-symlinks
   "*f" 'dired-mark-files-regexp
   "*s" 'dired-mark-subdir-files
   "*?" 'dired-unmark-all-files
   "*!" 'dired-unmark-all-marks
   "*t" 'dired-toggle-marks
   "*g" 'dired-mark-files-containing-regexp
   "*~" 'dired-flag-garbage-files
   "lo" 'do-dired-find-marked-files
   "lO" 'do-dired-find-marked-files-noframe
   "*o" 'do-dired-find-marked-files
   "*O" 'do-dired-find-marked-files-noframe

   "a" 'dired-toggle-read-only
   "A" 'dired-toggle-read-only
   "i" 'dired-toggle-read-only
   "I" 'dired-toggle-read-only)

  
  (use-package dired-open
	:ensure t
	:after (dired)
	:config
	(general-define-key
	 :keymaps 'dired-mode-map
	 :states 'normal
	 "C-<return>" 'dired-open-xdg))
  
  ;; (use-package dired-ranger
  ;; 	:ensure t
  ;;   :after (dired)
  ;;   :config
  ;;   (general-define-key
  ;;    :keymaps 'dired-mode-map
  ;;    :states 'normal
  ;;    "Y" 'dired-ranger-copy
  ;;    "M" 'dired-ranger-move
  ;;    "P" 'dired-ranger-paste))

  (use-package dired-collapse
	:ensure t
	:commands (dired-collapse-mode)
	;; :hook ((dired-mode . dired-collapse-mode))
	:init
	(general-define-key
	 :keymaps 'dired-mode-map
	 :states 'normal
	 "lc" 'dired-collapse-mode))

  (use-package dired-subtree
  	:ensure t
  	:commands (dired-subtree-insert
  			   dired-subtree-remove
  			   dired-subtree-beginning
  			   dired-subtree-end)
  	:init
  	(general-define-key
  	 :keymaps 'dired-mode-map
  	 :states 'normal
  	 "<tab>" 'dired-subtree-insert
  	 "<M-tab>" 'dired-subtree-remove
  	 "[" 'dired-subtree-beginning
  	 "]" 'dired-subtree-end
  	 "*}" 'dired-subtree-mark-subtree)
  	:config
  	(set-face-attribute 'dired-subtree-depth-1-face nil
  						:background chocolate-theme-shadow)
  	(set-face-attribute 'dired-subtree-depth-2-face nil
  						:background chocolate-theme-shadow+1)
  	(set-face-attribute 'dired-subtree-depth-3-face nil
  						:background chocolate-theme-shadow+2)
  	(set-face-attribute 'dired-subtree-depth-4-face nil
  						:background chocolate-theme-shadow+3)
  	(set-face-attribute 'dired-subtree-depth-4-face nil
  						:background chocolate-theme-shadow+3))

  (use-package openwith
	:ensure t
	:hook ((org-mode . openwith-mode)
		   (dired-mode . openwith-mode))
	:config
	(setq openwith-associations '(("\\.rar\\'" "file-roller" (file))
								  ("\\.zip\\'" "file-roller" (file))
								  ("\\.7z\\'" "file-roller" (file))
								  ("\\.gz\\'" "file-roller" (file))
								  ("\\.tar\\'" "file-roller" (file))
								  ("\\.pdf\\'" "zathura" (file)))))

  ;; For large file
  (use-package vlf
	:defer t
	:ensure t
	:config
	(require 'vlf-setup)))
