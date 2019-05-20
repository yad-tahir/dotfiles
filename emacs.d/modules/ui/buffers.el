;;; -*- lexical-binding: t; -*-

;; Add key bindings to manage buffers in the evil mode
(general-define-key
 :keymaps 'override
 :prefix "SPC b"
 :states '(normal visual)
 "" '(:ignore t :which-key "buffer")
 "n" 'evil-buffer-new
 "q" 'evil-delete-buffer
 "d" 'evil-delete-buffer
 "b" 'evil-prev-buffer
 "B" 'evil-next-buffer
 "l" 'ibuffer
 "s" 'evil-switch-to-windows-last-buffer)

(general-define-key
 :keymaps 'override
 :prefix "SPC s"
 :states '(normal visual)
 "b" 'ibuffer)

(general-define-key
 "<f5>" 'revert-buffer
 "<f3>" '(lambda() (interactive)
		   (evil-delete-buffer (current-buffer)))
 "S-<f3>" '(lambda() (interactive)
			 (evil-delete-buffer (current-buffer))
			 (evil-quit)))

;; Remove the need for a confirmation when process buffer is killed
(setq kill-buffer-query-functions
	  (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;;###autoload
(defun do--switch-to-buffer (org-fun &rest args)
  "Check if there is a window showing the buffer. If yes, raise its frame and
  switch to it. Otherwise, open the buffer in the current window.

The aim of this function is to minimize duplicated windows as much as possible."

  (ignore args)
  ;; Get the buffer
  (let ((b (get-buffer (format "%s" (car args)))))
	;; The scratch buffer is unique and always assigned to a window in
	;; emacs for some reason.
	(unless (equal "*scratch*" (buffer-name b))
	  ;; Get the window if the buffer exists
	  (let ((win (get-buffer-window b t)))
		  (when win
			;; Get the frame
			(raise-frame (window-frame win))
			;; Select the window
			(select-frame-set-input-focus (window-frame win)))))
	;; Otherwise, continue
	(apply org-fun args)))

(advice-add 'find-file :around 'do--switch-to-buffer)
(advice-add 'switch-to-buffer :around #'do--switch-to-buffer)
(with-eval-after-load 'ivy
  (advice-add 'ivy--switch-buffer-action :around #'do--switch-to-buffer))

(defun do--display-buffer (buf &rest args)
  "A custom display buffer function to avoid creating Emacs windows as much
as possible. Since we are using i3wm, there is no need for nested windows inside
Emacs. The buffer BUF will instead be display on the current window"
  (ignore args)
  (with-current-buffer buf
	(display-buffer-same-window buf 'nil)))
(advice-add 'display-buffer :override #'do--display-buffer)
