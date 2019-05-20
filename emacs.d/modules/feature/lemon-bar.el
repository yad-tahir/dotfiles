;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun do-lemonbar-notify(&rest args)
  "Sends an event to the lemonbar"
  (ignore args)
  (ignore-errors
	(start-process-shell-command
	 "lemonbar-notifier"
	 'nil
	 (concat "echo 'b"
			 (format "%s" (length (do-list-buffers)))
			 "'> /tmp/lemon-panel-fifo-emacs"))))

;; Notify lemon-bar whenever we create a new buffer, delete one, switch to another.
(advice-add 'switch-to-buffer :after #'do-lemonbar-notify)
(advice-add 'find-file :after #'do-lemonbar-notify)
(advice-add 'evil-delete-buffer :after #'do-lemonbar-notify)
(with-eval-after-load 'ivy
  (advice-add 'ivy-switch-buffer-kill :after #'do-lemonbar-notify))

;;;###autoload
(defun do-list-buffers (&optional list)
  "Return list of user buffers. This function is used in i3Block to
display the number of open buffers in Emacs."

  (interactive)
  (when (null list)
	(setq list (buffer-list)))
  (let ((result '()))
	(while list
	  (let* ((buffer (car list))
			 (name (buffer-name buffer)))
		(and name
			 (not (string-equal name ""))
			 (/= (aref name 0) ?\s)
			 (push buffer result)))
	  (setq list (cdr list)))
	result))

