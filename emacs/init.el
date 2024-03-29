;;Presentation
;;============

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; update the package metadata if the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;;to get rid of the tool and scroll bar for graphical GnuEmacs
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-7")))

;; Get rid of menu bar regardless
(menu-bar-mode -1)

;; Show column numbers
(setq column-number-mode t)

;;Nice-ish built-in dark theme
;; (load-theme 'tsdh-dark)
;; Otherwise:
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Previews hex colours in editor. Manually turn on for now
(use-package rainbow-mode)

;; Removes the curly arrows to denote word/line wraps
(setq-default fringe-indicator-alist ())
;; Set fringe width
(set-fringe-style 4)

;;usable light color scheme
;; (load-theme 'whiteboard)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))) ;; start maximised

;; Newline at end of file
(setq require-final-newline t)

;; Disable cursor blinking (required on Emacs 26 → 28 upgrade)
;; https://www.emacswiki.org/emacs/NonBlinkingCursor
(blink-cursor-mode 0)

;;Bindings
;;========

;; use shift + arrow keys to move between frames
;; https://www.reddit.com/r/emacs/comments/123lbu/there_must_be_a_better_way_to_switch_between/?st=jec01uww&sh=2b2f33ff
(global-set-key [(control j)]  'windmove-down)
(global-set-key [(control k)]  'windmove-up)
(global-set-key [(control h)]  'windmove-left)
(global-set-key [(control l)]  'windmove-right)

(global-set-key (kbd "<f2>") 'my/copy-to-system-clipboard)

;; Remapping from kill-region, which doesn't seem to be useful
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

;; Used in send-to-tmux.el as well
(defun my/get-copy-command ()
  (if (string-equal system-type "gnu/linux")
      (if (string-match "-[Mm]icrosoft" operating-system-release)
          ;; WSL (WSL1 has "-Microsoft", WSL2 has "-microsoft-standard")
          "clip.exe"
        ;; Linux
        "xsel -i -b")
    ;; MacOS
    "pbcopy"))

;; https://emacs.stackexchange.com/a/59607/19600
(defun wsl-paste ()
  (interactive)
  (let ((clipboard
         (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
    (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
    (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
    clipboard))

;; Used in yasnippet links (orgmode and markdown)
(defun my/get-clipboard-contents ()
  "Return system clipboard contents."
  (interactive)
  (if (string-equal system-type "gnu/linux")
      (if (string-match "-[Mm]icrosoft" operating-system-release)
          ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
          (wsl-paste)
        ;; Linux
        (shell-command-to-string "xclip -selection clipboard -o"))
    ;; MacOS
    (shell-command-to-string "pbpaste")))

(defun my/copy-to-system-clipboard ()
  "Copy region (or while buffer if no region selected) to the system clipboard."
  (interactive)
  (if (use-region-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) (my/get-copy-command))
        (deactivate-mark)
        (message "region copied to system clipboard"))
    (save-excursion
      (mark-whole-buffer)
      (shell-command-on-region (region-beginning) (region-end) (my/get-copy-commnd))
      (deactivate-mark)
      (message "buffer copied to system clipboard"))))

;;Behaviour
;;=========

;;save custom variables elsewhere:
;;http://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp
;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(let ((my-custom-file "~/.emacs-custom.el"))
  (unless (file-exists-p my-custom-file)
    (shell-command (concat "touch " my-custom-file)))
  (setq custom-file my-custom-file)
  (load my-custom-file))

; save minibuffer history across sessions
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode 1)

;; Stop that annoying beeping https://www.emacswiki.org/emacs/AlarmBell#toc3
(setq ring-bell-function 'ignore)

;;enabling sessions (disabling for now to encourage play around with Helm)
;;http://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
(desktop-save-mode 0)

;;https://www.emacswiki.org/emacs/ShowParenMode
;;see the matching parens
(show-paren-mode 1)
;;remove the delay
(setq show-paren-delay 0)

;; Use word-wrapping by default
(global-visual-line-mode t)

;;highlight the current line (turning it off because it won't make the search highlighting show up)
;;(global-hl-line-mode 1)
;;(set-face-foreground 'highlight nil)
;;(set-face-underline-p 'highlight t)

;;Use vertical split by default (turning it off now because I've decided I like it the default way)
;;http://stackoverflow.com/questions/7997590/how-to-change-the-default-split-screen-direction
;; Always vertical
;;(setq split-width-threshold nil)
;; Always horizontal
;;(setq split-width-threshold 0)

;; https://www.emacswiki.org/emacs/BackupDirectory
;; Stop *~ files littering repo
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)

(add-to-list 'load-path "~/dotfiles/emacs")
(require 'send-to-tmux)

;; disable killing to system clipboard by default
;; (setq select-enable-clipboard nil)

;;Tramp
;;https://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "ssh")
;; Getting around sshing into Linux OSes (Footnote 2) http://howardism.org/Technical/Emacs/literate-devops.html
(setq temporary-file-directory "/tmp")

;; Playing around with syntax tables
;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word

(defun underscore-as-word ()
  "Set underscore as part of word (e.g. for evil-visualstar)."
  (modify-syntax-entry ?_ "w"))

(defun hyphen-as-word ()
  "Set hyphen as part of word (e.g. for evil-visualstar)."
  (modify-syntax-entry ?- "w"))

(add-hook 'arduino-mode-hook 'underscore-as-word)
(add-hook 'org-mode-hook 'underscore-as-word)
(add-hook 'haxe-mode-hook 'underscore-as-word)
(add-hook 'markdown-mode-hook 'underscore-as-word)
(add-hook 'python-mode-hook 'underscore-as-word)
(add-hook 'js2-mode-hook 'underscore-as-word)
(add-hook 'gdscript-mode-hook 'underscore-as-word)

(add-hook 'cider-mode-hook 'hyphen-as-word)
(add-hook 'emacs-lisp-mode-hook 'hyphen-as-word)

;; For Chinese input
;; https://stackoverflow.com/questions/6344389/osx-emacs-unbind-just-the-right-alt
;; http://pinpinchinese.com/blog/how-to-type-pinyin-tone-marks-mac-os-x-yosemite/
(setq mac-right-option-modifier nil)

;; For iTerm2 (and xterm) compatability: https://www.emacswiki.org/emacs/iTerm2

(define-key input-decode-map "\e[1;P1" (kbd "C-)")) ;; smartparens slurp
(define-key input-decode-map "\e[1;P2" (kbd "C-=")) ;; expand-region
(define-key input-decode-map "\e[1;P3" (kbd "C-}")) ;; smartparens barf
;; (define-key input-decode-map "\e[1;P4" (kbd "M-[")) ;; sp-backward-unwrap-sexp

;;Packages
;;========
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;;Css
;;===
(setq css-indent-offset 2)

;;JavaScript
;;==========
(setq js-indent-level 2)

;;R
(use-package ess)
(setq ess-indent-offset 2)

;;Godot
(use-package gdscript-mode
  :hook (gdscript-mode . lsp))

;;Scala
(use-package scala-mode
  :hook (scala-mode . lsp))

(use-package lsp-metals)

;;Java
(use-package lsp-java
  :hook (java-mode . lsp))

(add-hook 'java-mode-hook
          (lambda()
            (setq c-basic-offset 2)))

(defun my/fix-ispell-contraction ()
  "Fixes contractions (e.g. shouldn't) aren't not being checked properly.
See https://github.com/casouri/lunarymacs/blob/master/star/checker.el#L44-L49."
  (add-to-list 'ispell-dictionary-alist
               '("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))

;; Flyspell
(use-package flyspell
  :ensure nil
  :config
  (add-hook 'flyspell-mode-hook 'my/fix-ispell-contraction)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom

  ;; Add correction to abbreviation table.
  ;; (flyspell-abbrev-p t)

  ;; Chosen from (prin1-to-string (ispell-valid-dictionary-list))
  (flyspell-default-dictionary "en_GB")

  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

;;LSP
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration))
(use-package lsp-ui)
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; Up from 800000 (800KB) to 100MB
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; From https://github.com/godotengine/emacs-gdscript-mode#known-issues
(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))
;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
(advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)

;;Lisp Development
;;================

;; Racket filetype detection
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;;(defvar scheme-program-name "guile")

;;this one looks more recommended:
;;http://www-users.cs.umn.edu/~gini/1901-07s/emacs_scheme/
(set-variable (quote scheme-program-name) "racket")

;;Misc functions
;;==============

(defun get-set-screenshot-location ()
  "Prints the current location of where screenshots are kept (MacOS specific)."
  (interactive)
  (let* ((current-location (shell-command-to-string "defaults read com.apple.screencapture location"))
         (prompt (format "Current screenshot location is: %s\nNew location: " current-location))
         (new-directory (read-directory-name prompt)))
    (shell-command (format "defaults write com.apple.screencapture location %s" new-directory))
    (shell-command "killall SystemUIServer")
    (message "New location set to %s" new-directory)))

;; https://stackoverflow.com/a/44489067/1010076
(defun my/show-copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (when file-name
        (message "%s copied to clipboard" (kill-new file-name)))))

;; (global-display-line-numbers-mode t)
;; (setq display-line-numbers 'relative)
;; (global-linum-mode 1)

;; In Ubuntu bash
;; https://stackoverflow.com/questions/5288213/how-can-i-paste-the-selected-region-outside-of-emacs/14659015#14659015
(use-package xclip
  :config
  (xclip-mode 1))

(require 'thingatpt)

;;Relative numbers
;;----------------
;; (use-package nlinum-relative
;;   :config

;;   ;; setup for evil
;;   (nlinum-relative-setup-evil)
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode)
;;   (add-hook 'text-mode-hook 'nlinum-relative-mode)
;;   (setq nlinum-relative-redisplay-delay 0.1)

;;   ;; or "" for display current line number
;;   (setq nlinum-relative-current-symbol "")

;;   ;; 1 if you want 0, 2, 3...
;;   (setq nlinum-relative-offset 0))

;; Either use this or relative numbers; undecided
(global-display-line-numbers-mode)

;;Packages
;;========
;;Install these with e.g. M-x package-install RET evil RET

;;Which key
;;===
(use-package which-key
  :config
  (which-key-mode))

;;Yaml
;;====
(use-package yaml-mode
  :mode
  (("\.yml" . yaml-mode)))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))

(defun my/replace-symbol-in-buffer ()
  "Replace symbol under cursor in entire buffer."
  (interactive)
  (let ((to-replace (thing-at-point 'word)))
    (setq unread-command-events
          (listify-key-sequence (format ":%%s/%s/" to-replace)))))

(defun my/replace-symbol-in-line ()
  "Replace symbol under cursor in current line."
  (interactive)
  (let ((to-replace (thing-at-point 'symbol)))
    (setq unread-command-events
          (listify-key-sequence (format ":s/%s/" to-replace)))))

(defun my/replace-symbol-in-selection ()
  "Replace symbol under cursor in evil visual selection."
  (interactive)
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((to-replace (thing-at-point 'symbol)))
      (evil-visual-restore)
      (setq unread-command-events
            (listify-key-sequence (format ":s/%s/" to-replace))))))

;; https://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun my/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun my/random-thing ()
  "Select randomly between list of things inputted by user."
  (interactive)
  (let* ((things-str (read-string "Enter things to be chosen randomly (separated by space): "))
         (things (split-string things-str))
         (thing-idx (random (length things))))
    (message
     (format "Random thing chosen: %s"
          (nth thing-idx things)))))

;;Evil Leader
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key

    "0" 'delete-window

    ;; "," 'evil-repeat-find-char-reverse
    "a" 'projectile-ag
    "b" 'helm-filtered-bookmarks
    "c" 'comment-line
    "d" 'magit-log-buffer-file
    "f" 'projectile-find-file
    "F" 'projectile-find-file-in-known-projects

    "g." 'git-gutter:next-hunk
    "g," 'git-gutter:previous-hunk
    "gp" 'git-gutter:popup-hunk
    "gr" 'git-gutter:revert-hunk

    "h" 'helm-apropos

    "l" 'helm-buffers-list

    "m" 'magit-status
    "n" 'dired-sidebar-toggle-sidebar ;; Like NERDTree
    "o" 'delete-other-windows

    "sc" 'send-to-tmux/set-config
    "sd" 'send-to-tmux/get-difference
    "sg" 'send-to-tmux/get-config
    "sp" 'send-to-tmux/set-pane
    "ss" 'send-to-tmux/send-snippet

    "rt" 'my/random-thing

    "rf" 'my/rename-file-and-buffer

    "rsb" 'my/replace-symbol-in-buffer
    "rsl" 'my/replace-symbol-in-line
    "rss" 'my/replace-symbol-in-selection

    "p" 'projectile-switch-project
    "y" 'my/show-copy-buffer-path
    "w" 'toggle-word-wrap))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt))

;;Evil
;;----
;;https://github.com/noctuid/evil-guide#switching-between-evil-and-emacs
;;Disable overriding some useful emacs bindings in insert mode. Needs to be defined before loading Evil
(defvar evil-insert-state-bindings
  '(("\C-v" . quoted-insert)
    ("\C-k" . evil-insert-digraph)
    ("\C-o" . evil-execute-in-normal-state)
    ("\C-r" . evil-paste-from-register)
    ("\C-y" . evil-copy-from-above)
    ;; ("\C-e" . move-end-of-line)
    ("\C-n" . evil-complete-next)
    ("\C-p" . evil-complete-previous)
    ("\C-x \C-n" . evil-complete-next-line)
    ("\C-x \C-p" . evil-complete-previous-line)
    ("\C-t" . evil-shift-right-line) ;;tempted to disable this pair as well
    ;; ("\C-d" . evil-shift-left-line)
    ;; ("\C-a" . move-beginning-of-line)
    ("\C-w" . evil-delete-backward-word)))

(defun evil-escape-and-save ()
  "Back to normal mode, then save."
  (interactive)
  (evil-normal-state)
  (save-buffer))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;;too much other crap going on to be worrying about evil here
  ;; (evil-set-initial-state 'comint-mode 'emacs)
  ;; (evil-set-initial-state 'sldb-mode 'emacs)
  ;; (evil-set-initial-state 'geiser-doc-mode 'emacs)
  ;; (evil-set-initial-state 'geiser-debug-mode 'emacs)
  ;; (evil-set-initial-state 'treemacs-mode 'emacs)

  ;; Unbind C-a from evil-ex-completion; can (use C-l, C-d or TAB instead)
  (define-key evil-ex-completion-map "\C-a" nil)

  (setq evil-move-cursor-back nil)
  (define-key evil-normal-state-map "\C-s" 'save-buffer)
  
  ;; Can't seem to redefine these in evil-insert-state-bindings, so doing them here
  (define-key evil-insert-state-map "\C-e" 'move-end-of-line)
  (define-key evil-insert-state-map "\C-a" 'move-beginning-of-line)
  (define-key evil-insert-state-map "\C-d" 'delete-char)
  (define-key evil-insert-state-map "\C-s" 'evil-escape-and-save))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init '(dired
                          magit
                          (package-menu package))))

;;evil-surround
;;-------------
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;evil-visualstar
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; https://github.com/7696122/evil-terminal-cursor-changer/issues/19
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :init
    (setq evil-motion-state-cursor 'box)  ; █
    (setq evil-visual-state-cursor 'box)  ; █
    (setq evil-normal-state-cursor 'box)  ; █
    (setq evil-insert-state-cursor 'bar)  ; ⎸
    (setq evil-emacs-state-cursor  'hbar) ; _
    :config
    (etcc-on)))

;;key-chord (For escaping normal mode)
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

;;Treemacs
;;========
;; (use-package treemacs
;;   :ensure t
;;   :config
;;   (progn
;;     (use-package treemacs-projectile
;;       :ensure t)
;;     (use-package treemacs-evil
;;       :ensure t)
;;     (setq treemacs-follow-mode t
;;           treemacs-git-mode 'simple)
;;     (global-set-key (kbd "M-L") (lambda ()
;;                                   (interactive)
;;                                   (treemacs-find-file)
;;                                   (treemacs-select-window)))))

;; Dired sidebar
(use-package dired-sidebar
  :bind (("M-L" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-theme 'nerd))

;; Bind dired-do-compress-to to this in evil-collection/modes/dired (L70)
;; Then run emacs-lisp-byte-compile-and-load
(defun my/dired-create-file-relative (file)
  "Create a file called FILE relative to the location of cursor in dired subtree.  If FILE already exists, signal an error."
  (interactive
   (list (read-file-name "Create file: " (dired-current-directory))))
  (let* ((expanded (expand-file-name file)))
    (message "%s: %s" "expanded" (prin1-to-string expanded))
    (if (file-exists-p expanded)
        (error "Cannot create file %s: file exists" expanded))

    ;; Writes the file onto dired subdirectory
    (write-region "" nil expanded t)
    (dired-add-file expanded)
    ;; Refreshes
    (revert-buffer)

    ;; Should goto the file
    (find-file expanded)))

;;Eyebrowse
;;---------
;; (use-package eyebrowse
;;   :init
;;   (setq eyebrowse-wrap-around 1)
;;   (setq eyebrowse-new-workspace 1)
;;   :config
;;   (eyebrowse-mode t)
;;   (eyebrowse-setup-opinionated-keys))

;;Perspective
;;-----------
;; (use-package perspective
;;   :config
;;   (persp-mode)
;;   (use-package persp-projectile))

;;Magit
;;-----
(use-package magit
  :ensure t)
;;https://magit.vc/manual/magit/Getting-started.html#Getting-started

;;Expand Selection
;;----------------
(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;; Slime
;;------
(use-package slime
  :config
  (setq inferior-lisp-program
        (if (string-equal system-type "gnu/linux")
            "/usr/bin/sbcl" ; Ubuntu
          "/usr/local/bin/sbcl" ; MacOS
          ))
  (slime-setup '(slime-fancy slime-company)))

(use-package slime-company)


;; Helm
;;-----
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (use-package helm-config)
  (setq helm-move-to-line-cycle-in-source t
        helm-ff-file-name-history-use-recentf t
        helm-mode-fuzzy-match t
        helm-candidate-number-limit 100)
  (helm-mode 1)
  (use-package helm-flx
    :init
    (use-package flx)
    :config
    (helm-flx-mode +1))
  (helm-adaptive-mode 1)
  ;; interferes with dired-sidebar https://emacs.stackexchange.com/questions/17077/how-can-i-skip-helm-ido-when-i-want-to-open-dired?rq=1
  (add-to-list 'helm-completing-read-handlers-alist
               '(dired-do-copy . nil))
  (add-to-list 'helm-completing-read-handlers-alist
               '(dired-do-rename . nil)))

;; Using Helm command completion instead of default

;;-----
;;use flx ido for buffer switching
;; http://ergoemacs.org/emacs/emacs_buffer_switching.html
;; (require 'ido)
;; (ido-mode t)
;; (ido-everywhere t)

;; (use-package flx-ido
;;   :ensure t
;;   :config
;;   (flx-ido-mode t))

;;Flycheck
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; Flycheck overrides some timestamping keybindings of orgmode - hence disabling
  (setq flycheck-global-modes '(not org-mode))
  
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
  	  '(javascript-jshint)))
  
  ;; use eslint with web-mode for js files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;;(flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)
  
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

;;Graphviz Dot Mode
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

;;Orgmode
;;-------
;;Scrap/misc
(add-to-list 'load-path "~/dotfiles/emacs/config")
(require 'ob-foobar)

(defun reload-ob-foobar ()
  "Reloads foobar for iterative development."
  (interactive)
  (progn
    (unload-feature 'ob-foobar 'force)
    (require 'ob-foobar)))

(defun reload-ob-core ()
  "Reloads ob-core for iterative development."
  (interactive)
  (progn
    (unload-feature 'ob-foobar 'force)
    (require 'ob-core)))


(use-package org
  :config
  (use-package ox-reveal)
  (use-package gnuplot-mode)
  (use-package htmlize)
  ;; Regain normal template expansion functionality
  (require 'org-tempo)
  ;; Load Markdown exporter automatically
  (require 'ox-md nil t))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-link-frame-setup '((file . find-file)))
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq org-default-notes-file "~/captured.org")
;; For Code blocks that produce code, redisplay it on evaluation
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;; Don't indent source block
(setq org-edit-src-content-indentation 0)
;; Get rid of footer when exporting html
(setq org-html-postamble nil)
(setq org-log-done t)
;;workflows:
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "OBSOLETE(o)")))
;; Allowing certain languages to be evaluated in code blocks: http://orgmode.org/manual/Languages.html#Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js . t)
   (python . t)
   (ditaa . t)
   (lilypond . t)
   (foobar . t)
   (dot . t)
   (shell . t)
   (ruby . t)
   (scheme .t)))

;; https://stackoverflow.com/questions/16770868/org-babel-doesnt-load-graphviz-editing-mode-for-dot-sources
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;;disable confirmation prompt for these languages
(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "emacs-lisp")
	   (string= lang "js")
	   (string= lang "shell")
	   (string= lang "python")
	   (string= lang "lilypond")
	   (string= lang "foobar")
	   (string= lang "dot")
	   (string= lang "scheme")
	   (string= lang "ruby")
	   (string= lang "dot")
	   (string= lang "ditaa"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
;; disable hiding the link
(setq org-descriptive-links nil)
;; Linewrap (for long paragraphs)
(setq org-startup-truncated nil)
;; Ditaa path. This is true for MacOS brew install
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.10/libexec/ditaa0_10.jar")
;; Required for pdflatex
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(when (file-directory-p "~/pdotfiles")
  (load "~/pdotfiles/emacs/latex-classes.el"))

;; Setting tags column to left-aligned in both org and org-agenda view
(setq org-tags-column 1)
(setq org-agenda-tags-column 1)

;;Geiser
;;------
(use-package geiser
  :config
  (setq geiser-active-implementations '(racket)))
  ;; (setq geiser-implementations-alist
  ;;       '(((regexp "\\.rkt$") racket))))
  ; http://www.nongnu.org/geiser/geiser_3.html#Customization-and-tips
  ;; (setq geiser-racket-binary "/usr/local/bin/racket"))

;https://www.emacswiki.org/emacs/ParEdit ;;Commenting out for now to try out smartparens
;; (require 'paredit)
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; (add-hook 'racket-mode-hook           #'enable-paredit-mode)

;;Smartparens
;;-----------
(use-package smartparens
  :bind (
   ;; Traversal
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ;; Traversal Depth Changing
   ;; Forward
   ("C-M-d" . sp-down-sexp)
   ("C-M-e" . sp-up-sexp) ;; Shadowing slime-end-of-defun

   ;; Backward
   ("C-M-a" . sp-backward-up-sexp) ;; Shadowing slime-beginning-of-defun
   ("C-M-u" . sp-backward-down-sexp)

   ;; Modification
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)

   ("C-M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ;; This is conflicting with pasting from the terminal
   ("C-M-k" . sp-kill-sexp)
   ("M-k" . sp-kill-hybrid-sexp)

   ;; Binding shadows transpose-sexps
   ("C-M-t" . sp-transpose-sexp)))

(use-package smartparens-config
  :config
  (smartparens-global-mode t))

;; (use-package evil-smartparens
;;   :init
;;   (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;;Projectile
;;----------
;;package-install <RET> projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (setq projectile-enable-caching t))

;; Overrides projectile commands with helm variants
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-ag
  :ensure t
  :config
  (custom-set-variables
   '(helm-ag-use-agignore t)))
(use-package ag
  :ensure t)

;;Yasnippet
;;---------
;;package-list-packages → yasnippet
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs
        '("~/dotfiles/emacs/snippets"))
  :config
  (yas-global-mode 1))

;; Markdown-mode
;; -------------
(use-package markdown-mode
  :ensure t)

;; Company-mode
;; ------------
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-global-modes '(not org-mode markdown-mode))
  (setq company-selection-wrap-around t
        company-show-numbers t)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  ;; remap off C-w to stop conflict with evil-delete-backward-word
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-l") 'company-show-location))

;; Company-tern
(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (company-mode))))

;; js2-mode
;; --------
(use-package js2-mode)

;; Using js2-mode with flow fork (Temporary React Native solution)
;; (load "~/dev/js2-mode/js2-mode.el")
;; (js2-mode)

(setq-default js2-strict-missing-semi-warning nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; xref-js2
;;https://github.com/nicolaspetton/xref-js2
;;https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html
(use-package xref-js2
  :config
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (add-hook 'js2-mode-hook (lambda ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

;; Commenting this out; seems to interfere with org-export when not installed
;; Indium
;; (use-package indium
;;   :hook
;;   (js-mode . indium-interaction-mode))

;; web-mode
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; adjust indents for web-mode to 2 spaces
(use-package web-mode
  :init
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    ;;; http://web-mode.org/
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2))
  :config
  (add-hook 'web-mode-hook  'my-web-mode-hook))
;; ignoring tabs as well
(setq-default indent-tabs-mode nil)

;; emmet mode
(use-package emmet-mode)

;; JSX
;; (use-package rjsx-mode
;;   :mode (("App.js" . rjsx-mode)
;;          ("\\(components\\|containers\\)\\/.*\\.js\\'" . rjsx-mode)))

(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)))

;; Tide (TS, TSX)
(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (define-key evil-normal-state-map(kbd "gd") 'tide-jump-to-definition)
    (define-key evil-normal-state-map(kbd "gb") 'tide-jump-back)
    (define-key evil-normal-state-map(kbd "gr") 'tide-rename-symbol)
    (company-mode +1))
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; Always use tide from node_modules
  ;; TODO: make this like how eslint falls back to system typescript
  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; Adding typescript extensions to web mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "ts" (file-name-extension buffer-file-name)))
              (setq-local web-mode-enable-auto-quoting nil) ;;disable autoquoting as you're apt to use expressions then
              (setup-tide-mode))))
;; Crapton of flycheck stuff:

;; disable tsx-tide since we prefer tslint
;; (setq-default flycheck-disabled-checkers
;;   (append flycheck-disabled-checkers
;; 	  '(tsx-tide)))

;; use local typescript-eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-tslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tslint (and root
                      (expand-file-name "node_modules/tslint/bin/tslint"
                                        root))))
    (when (and tslint (file-executable-p tslint))
      (setq-local flycheck-typescript-tslint-executable tslint))))
(add-hook 'flycheck-mode-hook #'my/use-tslint-from-node-modules)


;; Elpy
;; ----
(use-package elpy
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  ;; Ensures that it uses the homebrew version, as set in .bashrc
  (setq elpy-rpc-python-command "python"))

;; Octave Mode
;; -----------
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;Misc
;;====

;; csv-mode
(use-package csv-mode)

;; Hunspell
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;;Lilypond
;;========
(let ((lilypond-emacs-path "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp"))
  (if (file-exists-p lilypond-emacs-path)
      (progn
        (add-to-list 'load-path lilypond-emacs-path)
        (use-package lilypond-mode
          :config
          (autoload 'LilyPond-mode "lilypond-mode")
          (setq auto-mode-alist
                (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
          (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))))))

;;Rust
;;====
(use-package rust-mode)

;;Kotlin
;;====
(use-package kotlin-mode)


;;Arduino
;;=======
(use-package arduino-mode
  :defer t
  :config
  (setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode)
                              auto-mode-alist))
  (autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t))

;;EditorConfig
(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

;; http://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html
;;rtags
(use-package rtags
  :config
  (progn
    (rtags-enable-standard-keybindings)
    (evil-define-key 'normal c-mode-base-map (kbd "gd") 'rtags-find-symbol-at-point)
    (evil-define-key 'normal c-mode-base-map (kbd "gi") 'rtags-symbol-info)
    (evil-define-key 'normal c-mode-base-map (kbd "gs") 'rtags-display-summary)
    (evil-define-key 'normal c-mode-base-map (kbd "go") 'rtags-location-stack-back)))

;; This literally isn't autocompleting anything
;; (use-package company-rtags
;;   :config
;;   (progn
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)
;;     (push 'company-rtags company-backends)
;;     ))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Irony
(use-package irony
  :config
  (progn
    (setq company-clang-executable "/usr/bin/clang-10")
    ;; If irony server was never installed, install it.
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    ;; Use compilation database first, clang_complete as fallback.
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                      irony-cdb-clang-complete))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package company-irony-c-headers)

(use-package flycheck-irony
  :config
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(use-package company-irony
 :config
 (progn
   (eval-after-load 'company '(add-to-list 'company-backends
                                           '(company-irony-c-headers company-irony)
                                           ))))

(use-package irony-eldoc
  :config
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc)))

;; PHP
(use-package php-mode)

;; Clojure (Cider, Flycheck)
(use-package cider
  :defer t
  :config
  (helm-cider-mode 1)

  ;; Keep sp backward up binding
  :bind (("C-M-a" . sp-backward-up-sexp))
  
  :hook
  (cider-repl-mode-hook . eldoc-mode)
  (cider-mode . eldoc-mode))

(use-package flycheck-clj-kondo)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  ;; (setq git-gutter:update-interval 1) ;; default is 0; update only on save
  (global-git-gutter-mode t))

;; From https://www.emacswiki.org/emacs/NxmlMode#toc11
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun my/save-last-snipped ()
  "Only implemented on WSL so far.
   Moves last snipped png from Windows snip (<Win-Shift-S>) into dir specified by SNIPPED_DESTINATION keyword
   Use while in org file with keyword of something like:
   ,#+SNIPPED_DESTINATION: assets"
  (interactive)
  (let* ((snipped-directory "/mnt/c/Users/eddie/AppData/Local/Packages/MicrosoftWindows.Client.CBS_cw5n1h2txyewy/TempState/ScreenClip")
         ;; https://stackoverflow.com/questions/26514437/emacs-sort-list-of-directories-files-by-modification-date
         (dir-and-files-by-date (sort (directory-files-and-attributes snipped-directory)
                                      #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x)))))
         (images-by-date (seq-filter #'(lambda (attributes)
                                         (string-suffix-p ".png" (car attributes)))
                                     dir-and-files-by-date))
         (snipped-destination-dir (file-name-concat
                                   default-directory
                                   (cadr (car (org-collect-keywords '("SNIPPED_DESTINATION"))))))
         (most-recent-image-attributes (car images-by-date))
         ;; "(\"{185B5151-6C0D-4EAB-9A86-749C99DD8C6B}.png\" nil 1 1000 1000 (25827 58047 771340 800000) (25824 47670 798102 300000) (25824 47670 798102 300000) 127696 \"-rwxrwxrwx\" t 26740122787601836 14)"
         (image-name (car most-recent-image-attributes))
         (image-path (file-name-concat snipped-directory image-name))
         (destination-filename (read-file-name
                                (concat "Copying over " image-name " as: ")
                                (file-name-concat
                                 snipped-destination-dir
                                 (format-time-string "%Y-%m-%d-%H:%M:%S.png" (current-time)))))
         ;; Links want a leading ./
         (rel-destination-filename (concat
                                    "./"
                                    (file-relative-name destination-filename default-directory)))
         (copy-command (concat "cp " image-path " " destination-filename)))
    (shell-command copy-command)
    rel-destination-filename))

;; Copilot spike (https://github.com/zerolfx/copilot.el)
;; Requires editorconfig (and s)
(add-to-list 'load-path "/home/xdl/dev/third-party/copilot.el")
(require 'copilot)
;; If copilot-mode is enabled:
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; Otherwise, manual triggering:
(bind-key "C-x ." 'copilot-complete)
(bind-key "C-x ," 'copilot-clear-overlay)

