(setq inhibit-startup-screen t
	  initial-scratch-message 'nil
	  initial-major-mode 'fundamental-mode)


;;;###autoload
(defun do-scratch-buffer()
  (interactive)
  (do-make-frame "scratch-buffer")
  (switch-to-buffer (get-buffer-create "*scratch*")))
