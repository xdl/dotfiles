;;Presentation
;;============

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

;;to get rid of the tool and scroll bar for graphical GnuEmacs
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-14")))

;; Get rid of menu bar regardless
(menu-bar-mode -1)

;; Show column numbers
(setq column-number-mode t)

;;Nice-ish dark theme
(load-theme 'wombat)

;; Removes the curly arrows to denote word/line wraps
(setq-default fringe-indicator-alist ())
;; Set fringe width
(set-fringe-style 4)

;;usable light color scheme
;;(load-theme 'whiteboard)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))) ;; start maximised

;; Newline at end of file
(setq require-final-newline t)



;;Bindings
;;========

;; use shift + arrow keys to move between frames
;; https://www.reddit.com/r/emacs/comments/123lbu/there_must_be_a_better_way_to_switch_between/?st=jec01uww&sh=2b2f33ff
(global-set-key [(control j)]  'windmove-down)
(global-set-key [(control k)]  'windmove-up)
(global-set-key [(control h)]  'windmove-left)
(global-set-key [(control l)]  'windmove-right)

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

;;Tramp
;;https://www.emacswiki.org/emacs/TrampMode
(setq tramp-default-method "ssh")
;; Getting around sshing into Linux OSes (Footnote 2) http://howardism.org/Technical/Emacs/literate-devops.html
(setq temporary-file-directory "/tmp")

;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
(add-hook 'arduino-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'org-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;;Packages
;;========
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;;Css
;;===
(setq css-indent-offset 2)

;;Javascript
;;==========

(setq js-indent-level 2)

;;Lisp Development
;;================

;; Racket filetype detection
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;;(defvar scheme-program-name "guile")

;;this one looks more recommended:
;;http://www-users.cs.umn.edu/~gini/1901-07s/emacs_scheme/
(set-variable (quote scheme-program-name) "racket")

;;Misc
;;====
(add-to-list 'load-path "~/dotfiles/emacs/config")
(require 'ob-foobar)

;; (defun reload-ob-foobar ()
;;   "Reloads foobar for iterative development"
;;   (interactive)
;;   (unload-feature 'ob-foobar)
;;   (require 'ob-foobar))

(defun reload-ob-foobar ()
  "Reloads foobar for iterative development"
  (interactive)
  (progn
    (unload-feature 'ob-foobar 'force)
    (require 'ob-foobar)))

(defun reload-ob-core ()
  "Reloads ob-core for iterative development"
  (interactive)
  (progn
    (unload-feature 'ob-foobar 'force)
    (require 'ob-core)))

;;Misc functions
;;==============

(defun get-set-screenshot-location ()
  "Prints the current location of where screenshots are kept."
  (interactive)
  (let* ((current-location (shell-command-to-string "defaults read com.apple.screencapture location"))
         (prompt (format "Current screenshot location is: %s\nNew location: " current-location))
         (new-directory (read-directory-name prompt)))
    (shell-command (format "defaults write com.apple.screencapture location %s" new-directory))
    (shell-command "killall SystemUIServer")
    (message "New location set to %s" new-directory)))

;; (global-display-line-numbers-mode t)
;; (setq display-line-numbers 'relative)
;; (global-linum-mode 1)

;;Relative numbers
;;----------------
(require 'nlinum-relative)
(nlinum-relative-setup-evil)                    ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(add-hook 'text-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0.1)      ;; delay
(setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
(setq nlinum-relative-offset 0)                 ;; 1 if you want 0, 2, 3...

;;Packages
;;========
;;Install these with e.g. M-x package-install RET evil RET

;;Yaml
;;====
(use-package yaml-mode
  :mode
  (("\.yml" . yaml-mode)))

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;Evil Leader
(require 'evil-leader)
;;--------------------
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "a" 'projectile-ag
  "b" 'helm-filtered-bookmarks
  "c" 'org-capture
  "e" 'eyebrowse-print-mode-line-indicator
  "f" 'projectile-find-file
  "h" 'helm-apropos
  "F" 'projectile-find-file-in-known-projects
  "g" 'magit-status
  "l" 'helm-buffers-list
  "t" 'treemacs
  "p" 'projectile-switch-project
  "s" 'save-buffer
  "w" 'save-buffer)

(use-package evil-numbers
  :requires evil
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
    ("\C-d" . evil-shift-left-line)
    ;; ("\C-a" . move-beginning-of-line)
    ("\C-w" . evil-delete-backward-word)))

;; Can't seem to redefine these in evil-insert-state-bindings, so doing them here
(define-key evil-insert-state-map "\C-e" 'move-end-of-line)
(define-key evil-insert-state-map "\C-a" 'move-beginning-of-line)

(require 'evil)
(evil-mode 1)
;;too much other crap going on to be worrying about evil here
(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'sldb-mode 'emacs)
(evil-set-initial-state 'treemacs-mode 'emacs)
(setq evil-move-cursor-back nil)

;;evil-surround
;;-------------
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;evil-visualstar
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;;key-chord (For escaping normal mode)
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

;;Treemacs
;;========
(use-package treemacs
  :ensure t
  :config
  (progn
    (use-package treemacs-projectile
      :ensure t)
    (use-package treemacs-evil
      :ensure t)
    (setq treemacs-follow-after-init t
          treemacs-change-root-without-asking t)
    (treemacs-follow-mode t)
    (global-set-key (kbd "M-L") (lambda ()
                                  (interactive)
                                  (treemacs-find-file)
                                  (treemacs-select-window)))))

;;Eyebrowse
;;---------
(use-package eyebrowse
  :init
  (setq eyebrowse-wrap-around 1)
  (setq eyebrowse-new-workspace 1)
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

;;Magit
;;-----
(use-package magit
  :ensure t)
;;https://magit.vc/manual/magit/Getting-started.html#Getting-started
(use-package evil-magit
  :ensure t
  :requires magit)

;;Expand Selection
;;----------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Slime
;;------
(require 'slime)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Helm
;;-----
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (use-package helm-config)
  (setq helm-move-to-line-cycle-in-source t
        helm-ff-file-name-history-use-recentf t)
  (helm-mode 1))

;; Using Helm command completion instead of default

;;-----
;;use flx ido for buffer switching
;;http://ergoemacs.org/emacs/emacs_buffer_switching.html
(require 'ido)
(ido-mode t)
(ido-everywhere t)

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode t))

;;Flycheck

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(javascript-jshint)))


;; use eslint with web-mode for js files
(flycheck-add-mode 'javascript-eslint 'web-mode)

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
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;;Graphviz Dot Mode
(use-package graphviz-dot-mode)

;;Orgmode
;;-------

;;from David O'Toole's tutorial: http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(use-package org
  :config
  (use-package ox-reveal)
  (use-package htmlize)
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

;; Setting tags column to 1
(setq org-tags-column 1)

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
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-d" . sp-down-sexp)
   ("C-M-e" . sp-up-sexp)

   ("C-M-a" . sp-backward-up-sexp)
   ("C-M-u" . sp-backward-down-sexp)

   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-M-k" . sp-kill-sexp)
   ("M-k" . sp-kill-hybrid-sexp)))

(use-package smartparens-config
  :config
  (smartparens-global-mode t))

(use-package evil-smartparens
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

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
  :ensure t)
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
  :defer t)

;; Company-mode
;; ------------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;; Company-tern
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (company-mode)))

;; js2-mode
;; --------
(require 'js2-mode)
(setq-default js2-strict-missing-semi-warning nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; xref-js2
(require 'xref-js2)
;;https://github.com/nicolaspetton/xref-js2
;;https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; Indium
(use-package indium
  :defer t)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)
;; ignoring tabs as well
(setq-default indent-tabs-mode nil)

;; JSX
(use-package rjsx-mode
  :mode (("\\(components\\|containers\\)\\/.*\\.js\\'" . rjsx-mode)))

;; Tide (TS, TSX)
(require 'tide)
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

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Always use tide from node_modules
;; TODO: make this like how eslint falls back to system typescript
(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

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

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
;;(flycheck-add-next-checker 'tsx-tide '(warning . typescript-tslint) 'append)

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
(require 'elpy)
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
;; Ensures that it uses the homebrew version, as set in .bashrc
(setq elpy-rpc-python-command "python")

;; Octave Mode
;; -----------
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;Misc
;;====

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

;;rtags
(use-package rtags
  :defer t
  :config
  (progn
    (rtags-enable-standard-keybindings)
    (evil-define-key 'normal c-mode-base-map (kbd "gd") 'rtags-find-symbol-at-point)
    (evil-define-key 'normal c-mode-base-map (kbd "gi") 'rtags-symbol-info)
    (evil-define-key 'normal c-mode-base-map (kbd "gs") 'rtags-display-summary)
    (evil-define-key 'normal c-mode-base-map (kbd "go") 'rtags-location-stack-back)))
