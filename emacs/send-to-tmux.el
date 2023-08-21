;;;; package --- Summary
;;; Commentary:
;;; Code:

;; N.B. requires the my/get-copy-command defined in init.el

;; For string-trim
(require 'subr-x)

;; https://superuser.com/questions/385472/get-current-window-number-for-bash-prompt
;; Get the window number of the current Emacs instance.
(defconst send-to-tmux/get-window-number-cmd "tmux display-message -p '#I'")
;; (defconst send-to-tmux/display-panes-cmd "tmux display-panes")

(defun send-to-tmux/get-window-number ()
  "Get the tmux window number of where current Emacs process resides."
  (string-trim (shell-command-to-string send-to-tmux/get-window-number-cmd)))

(defun send-to-tmux/get-smart-target-pane-config ()
  "Get the target pane for most scenarios."
  (format "%s.1" (string-trim (shell-command-to-string send-to-tmux/get-window-number-cmd))))

;; (defvar send-to-tmux/config '("default" "0.1" 1))
;; Can tmux window number can change duing an Emacs instance? If so this needs to be a function.
;; (session-name pane-config truncate-difference)
(defvar send-to-tmux/config `("default" ,(send-to-tmux/get-smart-target-pane-config) 1))
(defconst send-to-tmux/paste-path "~/.slime_paste")
(defvar send-to-tmux/last-known-pane-length 0)

(defun send-to-tmux/preprocess-region ()
  "Trims the selected region before sending it over to tmux."
  (let ((contents-trimmed (string-trim (buffer-substring (region-beginning) (region-end)))))
    (cond ((derived-mode-p 'python-mode)
           (if (= (length (split-string contents-trimmed "\n")) 1)
               (format "%s\n" contents-trimmed)
             (progn
              ;; (format "%s\n\n" contents-trimmed)))) ;; WSL workaround/alternative https://stackoverflow.com/a/38419584
               "%paste\n")))
               (shell-command-on-region (region-beginning) (region-end) (my/get-copy-command))
          (t (format "%s\n" contents-trimmed)))))


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
  "Load the contents of .slime_paste into the paste buffer, in preparation to be pasted."
  (format "tmux -L %s load-buffer %s"
          (car send-to-tmux/config)
          send-to-tmux/paste-path))

(defun send-to-tmux/paste-buffer-cmd ()
  "Paste the region into the target pane, thus evaluating it."
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

(defun send-to-tmux/get-config ()
  "Get send-to-tmux config."
  (interactive)
  (message
   (send-to-tmux/config-str)))

(defun send-to-tmux/config-str ()
  "Return readable tmux config."
  (format "{session name: %s, window.pane: %s, difference-trim: %s}"
          (nth 0 send-to-tmux/config)
          (nth 1 send-to-tmux/config)
          (nth 2 send-to-tmux/config)))

(defun send-to-tmux/set-config ()
  "Configure send-to-tmux config."
  (interactive)
  (let* ((session (car send-to-tmux/config))
         (session-prompt "session name: ")
         (window-pane (send-to-tmux/get-smart-target-pane-config))
         (window-pane-prompt "window.pane config: ")
         (diff-truncate (nth 2 send-to-tmux/config))
         (diff-truncate-prompt "diff truncate: ")
         (new-session (read-string session-prompt session))
         (new-pane (read-string window-pane-prompt window-pane))
         (new-diff-truncate (read-string diff-truncate-prompt
                                         (number-to-string diff-truncate))))
    (setq send-to-tmux/config (list new-session
                                    new-pane
                                    (string-to-number new-diff-truncate)))
    (message "tmux config set to: %s" (send-to-tmux/config-str))))

(defun send-to-tmux/set-pane ()
  "Shorthand for setting tmux pane."
  (interactive)
  (let* ((window-pane (format "%s." (send-to-tmux/get-window-number)))
         (window-pane-prompt "window.pane config: ")
         (new-pane (read-string window-pane-prompt window-pane)))
    (setq send-to-tmux/config (list (nth 0 send-to-tmux/config)
                                    new-pane
                                    (nth 2 send-to-tmux/config)))
    (message "tmux config set to: %s" (send-to-tmux/config-str))))

(provide 'send-to-tmux)
;;; send-to-tmux ends here
