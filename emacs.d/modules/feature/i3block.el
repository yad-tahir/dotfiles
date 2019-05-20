;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun do-i3block-notify(&rest args)
  "Sends SIG 14 to the i3block process in order to notify the emacs component"
  (ignore args)
  (ignore-errors
	(start-process-shell-command "pkill-i3block"
								 'nil "/usr/bin/pkill -SIGRTMIN+14 i3blocks")))

;; Notify i3block whenever we create a new buffer, delete one, switch to another.
(advice-add 'switch-to-buffer :after #'do-i3block-notify)
(advice-add 'find-file :after #'do-i3block-notify)
(advice-add 'evil-delete-buffer :after #'do-i3block-notify)
(with-eval-after-load 'ivy
  (advice-add 'ivy-switch-buffer-kill :after #'do-i3block-notify))

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

