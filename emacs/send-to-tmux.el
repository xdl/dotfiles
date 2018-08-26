(defvar send-to-tmux/config '("default" "0.1" 1))
(defconst send-to-tmux/paste-path "~/.slime_paste")
(defvar send-to-tmux/last-known-pane-length 0)

(defun send-to-tmux/preprocess-region ()
  "Trims the selected region before sending it over to tmux."
  (let ((contents (buffer-substring (region-beginning) (region-end))))
    (format "%s\n" (string-trim contents))
    ))

(defun send-to-tmux/postprocess-difference-result (diff)
  "Add a leading newline to DIFF before giving it back."
  (format "\n%s" diff))

;; https://emacs.stackexchange.com/questions/34283/percentage-in-format-string
;; https://stackoverflow.com/a/12524345
(defun send-to-tmux/line-length-cmd ()
  "Get number of lines in the target tmux buffer, stripping trailing newlines."
  (format "printf '%%s' \"$(tmux -L %s capture-pane -pS -32768 -t %s)\" | wc -l"
          (car send-to-tmux/config)
          (cadr send-to-tmux/config)))

(defun send-to-tmux/load-buffer-cmd ()
  "Load the region into the target pane, thus evaluating it."
  (format "tmux -L %s load-buffer %s"
          (car send-to-tmux/config)
          send-to-tmux/paste-path))

(defun send-to-tmux/paste-buffer-cmd ()
  "Paste the region into a scratch file, in preparation to be read back in."
  (format "tmux -L %s paste-buffer -d -t %s"
          (car send-to-tmux/config)
          (cadr send-to-tmux/config)))

(defun send-to-tmux/get-last-n-lines-cmd (n)
  "Get the last N lines from target pane, then apply the truncation diff offset."
  (format "printf '%%s' \"$(tmux -L %s capture-pane -pS -32768 -t %s)\" | tail -n %d | head -n %d"
          (car send-to-tmux/config)
          (cadr send-to-tmux/config)
          n
          (- n (nth 2 send-to-tmux/config))))

;; Functions to be exposed
(defun send-to-tmux/get-difference ()
  "Get difference of command."
  (interactive)
  (let* ((current-pane-length
          (string-to-number
           (shell-command-to-string (send-to-tmux/line-length-cmd))))
         (difference (- current-pane-length send-to-tmux/last-known-pane-length)))
    (save-excursion
      (end-of-line)
      (insert (send-to-tmux/postprocess-difference-result
               (shell-command-to-string
                (send-to-tmux/get-last-n-lines-cmd difference)))))))

(defun send-to-tmux/send-snippet ()
  "Send snippet to tmux."
  (interactive)
  (if (use-region-p)
      (progn
        (write-region (send-to-tmux/preprocess-region) nil send-to-tmux/paste-path)
        (deactivate-mark))
    (save-excursion
      (mark-paragraph)
      (write-region (send-to-tmux/preprocess-region) nil send-to-tmux/paste-path)
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
         (new-diff-truncate (read-string diff-truncate-prompt
                                         (number-to-string diff-truncate))))
    (setq send-to-tmux/config (list new-session
                                    new-pane
                                    (string-to-number new-diff-truncate)))
    (message "tmux config set to: %s" (prin1-to-string send-to-tmux/config))))

(provide 'send-to-tmux)

