(defvar send-to-tmux/config '("default" "0.1" 1))
(defconst send-to-tmux/paste-path "~/.slime_paste")
(defvar send-to-tmux/last-known-pane-length 0)

(defun send-to-tmux/line-length-cmd ()
  (format "tmux -L %s capture-pane -pS -32768 -t %s | wc -l"
          (car send-to-tmux/config)
          (cadr send-to-tmux/config)
          ))

(defun send-to-tmux/capture-pane-cmd ()
  (format "tmux -L %s capture-pane -pS -32768 -t %s"
          (car send-to-tmux/config)
          (cadr send-to-tmux/config)
          ))

(defun send-to-tmux/load-buffer-cmd ()
  (format "tmux -L %s load-buffer %s"
          (car send-to-tmux/config)
          send-to-tmux/paste-path))

(defun send-to-tmux/paste-buffer-cmd ()
  (format "tmux -L %s paste-buffer -d -t %s"
          (car send-to-tmux/config)
          (cadr send-to-tmux/config)))

(defun send-to-tmux/get-difference ()
  "Get difference of command."
  (interactive)
  (setq send-to-tmux/last-known-pane-length
        (string-to-number
         (shell-command-to-string (send-to-tmux/line-length-cmd))))
  (message "%s: %s" "send-to-tmux/last-known-pane-length" (prin1-to-string send-to-tmux/last-known-pane-length))
  )

(defun send-to-tmux/send-snippet ()
  "Send snippet to tmux."
  (interactive)
  (if (use-region-p)
      (progn
        (write-region (region-beginning) (region-end) send-to-tmux/paste-path)
        (deactivate-mark))
    (save-excursion
      (mark-paragraph)
      (write-region (region-beginning) (region-end) send-to-tmux/paste-path)
      (deactivate-mark)))
  (setq send-to-tmux/last-known-pane-length
        (string-to-number 
         (shell-command-to-string (send-to-tmux/line-length-cmd))))
  (shell-command (send-to-tmux/load-buffer-cmd))
  (shell-command (send-to-tmux/paste-buffer-cmd)))

(defun send-to-tmux/set-config ()
  "Configure send-to-tmux config."
  (interactive)
  (let* ((session (car send-to-tmux/config))
         (session-prompt "session name: ")
         (pane (cadr send-to-tmux/config))
         (pane-prompt "pane config: ")
         (diff-truncate (nth 2 send-to-tmux/config))
         (diff-truncate-prompt "diff truncate: ")
         (new-session (read-string session-prompt session))
         (new-pane (read-string pane-prompt pane))
         (new-diff-truncate (read-string diff-truncate-prompt diff-truncate)))
    (setq send-to-tmux/config (list new-session
                                    new-pane
                                    (string-to-number new-diff-truncate)))
    (message "tmux config set to: %s" (prin1-to-string send-to-tmux/config))
    ))
