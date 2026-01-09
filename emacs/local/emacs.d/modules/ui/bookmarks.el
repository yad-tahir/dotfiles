(require 'bookmark)

(general-define-key
 :keymaps 'override
 :prefix "SPC m"
 :states '(normal visual)
 "" '(:ignore t :which-key "bookmarks")
 "m" 'bookmark-set
 "M" 'bookmark-delete
 "a" 'bookmark-set
 "d" 'bookmark-delete
 "u" 'bookmark-rename
 "U" 'bookmark-relocate
 "l" 'bookmark-jump
 "L" 'bookmark-jump-other-frame
 "w" 'bookmark-save)

(general-define-key
 :keymaps 'override
 :states '(normal visual)
 "SPC '"  'bookmark-jump)


(setq bookmark-save-flag 1) ;; auto-save after every record modification

(provide 'do-bookmarks)
