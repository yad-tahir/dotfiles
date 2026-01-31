;;; package -- my vertico settings -*- lexical-binding: t; -*-

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
;; My vertico related settings

;;; Code:

(use-package vertico
  :ensure t
  :after minibuffer
  :functions (vertico--candidate vertico-insert)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 15)
  (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  (vertico-preselect 'directory) ;; Disable quark selections
  :init
  (vertico-mode)
  :config
  (general-define-key
   :keymaps 'vertico-map
   "TAB" 'minibuffer-complete-word
   "/" 'do--vertico-path-insert)
  (setq completion-styles '(basic substring partial-completion flex))

  (defun do--vertico-path-insert ()
    (interactive)
    (let* ((mb (minibuffer-contents-no-properties))
           (lc (if (string= mb "") mb (substring mb -1))))
      (cond ((string-match-p "^[/~:]" lc) (self-insert-command 1 ?/))
            ((file-directory-p (vertico--candidate)) (vertico-insert))
            (t (self-insert-command 1 ?/))))))

(use-package vertico-directory
  :after vertico
  :ensure nil ; It's part of the Vertico package
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names (e.g., /foo/bar/~/ becomes ~/)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :functions (marginalia--buffer-file
              marginalia--bookmark-type marginalia--truncate
              bookmark-prop-get marginalia--full-candidate
              marginalia--file-size marginalia--time)
  :config
  (setq marginalia-align 'right)

  (define-advice marginalia-annotate-buffer (:around (_org-fn cand))
    "Annotate buffer CAND with modification status, file name and major mode."
    ;; Emacs 31: `project--read-project-buffer' uses `uniquify-get-unique-names'
    (when-let* ((buffer (or (and (stringp cand)
                                 (get-text-property 0 'uniquify-orig-buffer cand))
                            (get-buffer cand))))
      (if (buffer-live-p buffer)
          (marginalia--fields
           ((marginalia--buffer-file buffer)
            :face 'marginalia-file-name))
        (marginalia--fields ("(dead buffer)" :face 'error)))))


  (define-advice marginalia-annotate-bookmark (:around (_org-fn cand))
    "Annotate bookmark CAND with its file name and front context string."
    (when-let* ((bm (assoc cand (bound-and-true-p bookmark-alist))))
      (marginalia--fields
       ((marginalia--bookmark-type bm) :width 10 :face 'marginalia-type)
       ((or (bookmark-prop-get bm 'filename)
            (bookmark-prop-get bm 'location))
        :truncate (if (bookmark-prop-get bm 'filename) -0.9 1)
        :face 'marginalia-file-name))))

  (define-advice marginalia--annotate-local-file (:around (_org-fn cand))
    "Annotate local file CAND."
    (marginalia--in-minibuffer
      (when-let* ((attrs (ignore-errors
                           ;; may throw permission denied errors
                           (file-attributes (substitute-in-file-name
                                             (marginalia--full-candidate cand))
                                            'integer))))
        (if (eq marginalia-align 'right)
            (marginalia--fields
             ;; File owner at the left
             ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
             ((marginalia--time (file-attribute-modification-time attrs))
              :face 'marginalia-date :width -12))
          (marginalia--fields
           ((marginalia--file-size attrs) :face 'marginalia-size :width -7)
           ((marginalia--time (file-attribute-modification-time attrs))
            :face 'marginalia-date :width -12)))))))

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :after vertico
  :functions (projectile-project-root
              consult--read consult--file-preview)
  :init
  (general-define-key
   :keymaps 'override
   :states 'normal
   "SPC sf" 'do-consult-rg-find
   "SPC sF" 'consult-find
   "SPC sg" 'consult-ripgrep
   "SPC sb" 'consult-buffer)

  (general-define-key
   :states 'normal
   "g/" 'consult-line)

  :config
  (require 'projectile)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  (setq consult-preview-key "M-<tab>"
        consult-narrow-key  "<"
        ;; Badly needed for evil-ex completions
        completion-in-region-function #'consult-completion-in-region)

  (defun do-consult-rg-find ()
    "Search for files using `rg --files' asynchronously."
    (interactive)
    (let* ((default-directory (if (fboundp 'projectile-project-root)
                                  (or (projectile-project-root) default-directory)
                                default-directory)))
      (find-file
       (consult--read
        (consult--process-collection
            (lambda (input)
              (list "/usr/bin/rg"
                    "--files"
                    "--color=never"
                    "--hidden"
                    "--glob" "!.git/*"
                    "--ignore-case"
                    "--glob" (concat "*" input "*"))))
        :prompt "Find File (rg): "
        :category 'file
        :sort nil
        :require-match t
        :state (consult--file-preview))))))

(use-package org-roam
  :functions (consult--process-collection
                 org-roam-node-list org-roam-node-read
                 org-roam--format-nodes-using-function
                 org-roam--format-nodes-using-template
                 do--org-roam-node-read-consult
                 do--org-roam-nodes-consult
                 do--org-roam-nodes-consult-narrow)
  :after (consult)
  :config
  (general-define-key
   :keymaps 'override
   :states 'normal
   "SPC nf" 'do-org-roam-files-consult
   "SPC ng" 'do-org-roam-grep-consult)

  (defun do-org-roam-grep-consult ()
    "Grep in node files using `rg'"
    (interactive)
    (let ((default-directory (or org-roam-directory default-directory)))
      (consult-ripgrep default-directory)))

  (defun do-org-roam-files-consult ()
    "Search for node files using `rg --files'"
    (interactive)
    (let* ((default-directory (or org-roam-directory default-directory))
           (cmd "rg --files --color=never --hidden --ignore-case ")
           ;; Run the command and split output into a list of lines
           (files (split-string (shell-command-to-string cmd) "\n" t)))
      (find-file
       (consult--read files
                      :prompt "Node file: "
                      :category 'file
                      :sort nil
                      :require-match t
                      :state (consult--file-preview)))))

  (defun do--org-roam-nodes-consult (filter-fn sort-fn)
    "Return org-roam-nodes list

Apply `org-roam-node-display-template', FILTER-FN, SORT-FN, and attach hidden
metadata for consult."
    (let* ((nodes (org-roam-node-list))
           (nodes (if filter-fn
                      (cl-remove-if-not
                       (lambda (n) (funcall filter-fn n))
                       nodes)
                    nodes))
           (nodes (if (functionp org-roam-node-display-template)
                      (org-roam--format-nodes-using-function nodes)
                    (org-roam--format-nodes-using-template nodes)))

           (sort-fn (or sort-fn
                        (when org-roam-node-default-sort
                          (intern (concat
                                   "org-roam-node-read-sort-by-"
                                   (symbol-name org-roam-node-default-sort))))))
           (nodes (if sort-fn (seq-sort sort-fn nodes)
                    nodes))

           ;; Attach consult hidden metadata
           (nodes (mapcar
                   (lambda (entry)
                     (let* ((title (car entry))  ;; Extract Title string
                            (node (cdr entry))   ;; Extract Node object
                            (tags (org-roam-node-tags node))
                            (category (org-roam-node-category node))
                            (hidden-parts
                             (delq nil
                                   (append
                                    (when (and category (not (string-empty-p category)))
                                      (list (concat "#" (downcase category))))
                                    (mapcar (lambda (tag) (concat ":" tag)) tags)))))
                       (if hidden-parts
                           (concat (propertize title 'node node)
                                   (propertize
                                    (concat " " (string-join hidden-parts " "))
                                    'invisible t
                                    'node node))
                         (propertize title 'node node))))
                   nodes)))
      nodes))

  (defun do--org-roam-nodes-consult-narrow ()
    "Narrowing configuration with strict filtering logic."
    (list
     :predicate ;; The logic that actually filters the list
     (lambda (cand)
       (if-let ((node (get-text-property 0 'node cand)))
           (let ((level (org-roam-node-level node))
                 ;; Get the raw string to check if it's an alias or title
                 (cand-str (substring-no-properties cand)))
             (cond
              ((eq consult--narrow ?h)
               (> level 0))
              ((eq consult--narrow ?a)
               (and (= level 0)
                    (cl-loop for alias in (org-roam-node-aliases node)
                             thereis (string-prefix-p alias cand-str))))
              ((eq consult--narrow ?t)
               (and (= level 0)
                    (not (cl-loop for alias in (org-roam-node-aliases node)
                                  thereis (string-prefix-p alias cand-str)))))
              (t t)))
         t))
     :keys
     (list (cons ?t "Title")
           (cons ?h "Headlines")
           (cons ?a "Alias"))))

  (defun do--consult-node-group (cand transform)
    "Group by Title, Headlines, or Alias."
    (if transform
        cand
      (if-let ((node (get-text-property 0 'node cand)))
          (let ((level (org-roam-node-level node))
                ;; Clean the candidate string for comparison
                (cand-str (substring-no-properties cand)))
            (cond
             ((> level 0) "Headlines")
             ((cl-loop for alias in (org-roam-node-aliases node)
                       thereis (string-prefix-p alias cand-str))
              "Alias")
             (t "Title")))
        "Ungrouped")))

  (defun do--org-roam-node-read-consult (initial-input
                                         &optional filter-fn sort-fn
                                         require-match)
    "Drop-in replacement for org-roam-node-read with Consult support."
    (let* ((candidates (do--org-roam-nodes-consult filter-fn sort-fn))
           (selected (consult--read
                      candidates
                      :prompt "Node: "
                      :initial initial-input
                      :category 'org-roam-node
                      :require-match require-match
                      :sort nil
                      :narrow (do--org-roam-nodes-consult-narrow)
                      ;; :group 'do--consult-node-group
                      :state (lambda (action cand)
                               (when (and cand (eq action 'preview))
                                 ;; CAND might not an org-node,
                                 ;; e.g. a mere string typed in the prompt
                                 (let ((file (ignore-errors
                                               (org-roam-node-file cand))))
                                   (funcall
                                    (consult--file-preview) 'preview file))))
                      :lookup (lambda (selected-text candidates &rest _)
                                (if-let ((node
                                          (cl-loop
                                           for item in candidates
                                           when (string=
                                                 (substring-no-properties item)
                                                 selected-text)
                                           return
                                           (get-text-property 0 'node item))))
                                    node
                                  selected-text))))) ;; non-node string
      (cond
       ((null selected) ;; e.g. C-g
        nil)
       ((not (stringp selected))
        selected)
       (t ;; New Node Typed if it not an existing node
        (org-roam-node-create :title (substring-no-properties selected))))))

  (advice-add #'org-roam-node-read :override #'do--org-roam-node-read-consult)

  (defun do--marginalia-annotate-roam-node (cand)
    "Annotate roam node with its filetags property."
    (when-let ((node (get-text-property 0 'node cand)))
      (let* ((tags (org-roam-node-tags node))
             (mtime (org-roam-node-file-mtime node))
             (tag-str (if tags
                          (mapconcat (lambda (tag) (concat "#" tag)) tags " ")
                        "")))

        (marginalia--fields
         (tag-str :width -20 :face 'marginalia-list)
         ((marginalia--time mtime) :face 'marginalia-date :width -12)))))

  (add-to-list 'marginalia-annotators
               '(org-roam-node do--marginalia-annotate-roam-node builtin none)))

(provide 'do-vertico)

;;; vertico.el ends here
