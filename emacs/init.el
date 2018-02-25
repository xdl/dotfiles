;;Presentation
;;============

;;to get rid of the tool and scroll bar for graphical GnuEmacs
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-15")))

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

;;Bindings
;;========

;; use shift + arrow keys to move between frames
(windmove-default-keybindings)

;;Behaviour
;;=========

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
      t)
  (package-initialize))

;;save custom variables elsewhere:
;;http://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp
;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(let ((my-custom-file "~/.emacs-custom.el"))
  (when (file-exists-p my-custom-file)
    (setq custom-file my-custom-file)
    (load my-custom-file)))

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
;;(setq split-width-threshold nil)
;;(setq split-width-threshold 0)

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

;;Packages
;;========
;;Install these with e.g. M-x package-install RET evil RET

;;Speedbar
;;========
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t)


;;Evil Leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "b" 'helm-filtered-bookmarks
  "f" 'helm-buffers-list
  "g" 'magit-status
  "s" 'sr-speedbar-toggle)

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
    ;; ("\C-e" . evil-copy-from-below)
    ("\C-n" . evil-complete-next)
    ("\C-p" . evil-complete-previous)
    ("\C-x \C-n" . evil-complete-next-line)
    ("\C-x \C-p" . evil-complete-previous-line)
    ("\C-t" . evil-shift-right-line) ;;tempted to disable this pair as well
    ("\C-d" . evil-shift-left-line)
    ;; ("\C-a" . evil-paste-last-insertion)
    ("\C-w" . evil-delete-backward-word)))
(require 'evil)
(evil-mode 1)
;;too much other crap going on to be worrying about evil here
(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'sldb-mode 'emacs)

;;Evil escape
;;-----------
;;https://github.com/syl20bnr/evil-escape
(require 'evil-escape)
(evil-escape-mode t)
(setq-default evil-escape-key-sequence "jk")
(setq evil-escape-excluded-major-modes
      '(comint-mode
	sldb-mode)) ;;Make sure this syncs up with any evil-set-initial-state calls

;;Relative numbers
;;----------------
(require 'nlinum-relative)
(nlinum-relative-setup-evil)                    ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(add-hook 'text-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)      ;; delay
(setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
(setq nlinum-relative-offset 0)                 ;; 1 if you want 0, 2, 3...

;;Magit
;;-----
(require 'magit)
;;https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-set-key (kbd "C-x g") 'magit-status)

;;Rainbow Delimiters
;;------------------
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;Expand Selection
;;----------------
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Slime
;;------
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Helm
;;-----
;;package-install <RET> helm
(require 'helm-config)
(helm-mode 1)
;; Using Helm command completion instead of default
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-h a") 'helm-apropos) ;;overriding apropos command
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
;; (setq helm-ff-skip-boring-files t)
;; (setq helm-boring-file-regexp-list
;;       '("node_modules$"))
;; For surfraw
;; https://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable
(setq exec-path
      (append exec-path
	      '("/usr/local/bin/")))
      
;; Ido (trying out Helm since it's got fuzzy finding)
;;-----
;;use ido for buffer switching
;;http://ergoemacs.org/emacs/emacs_buffer_switching.html
;;(require 'ido)
;;(ido-mode t)

;;Flycheck
(require 'flycheck)
(global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(javascript-jshint)))


;; use eslint with web-mode for jsx files
(add-hook 'js2-mode-hook 'flycheck-mode)

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

;;Orgmode
;;-------

;;from David O'Toole's tutorial: http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
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
   (sh . t)
   (ruby . t)
   (scheme .t)))

;;disable confirmation prompt for these languages
(defun my-org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "emacs-lisp")
	   (string= lang "js")
	   (string= lang "python")
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
(require 'geiser)
; http://www.nongnu.org/geiser/geiser_3.html#Customization-and-tips
(setq geiser-racket-binary "/usr/local/bin/racket")

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

(require 'smartparens-config)
(smartparens-global-mode t)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

;; Taken from here: https://github.com/Fuco1/smartparens/wiki/Working-with-expressions

;;Navigation
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)

(define-key sp-keymap (kbd "C-M-a") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-down-sexp)

;;Manipulation
(define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-}") 'sp-forward-barf-sexp)

(define-key sp-keymap (kbd "M-[") 'sp-backward-unwrap-sexp)
(define-key sp-keymap (kbd "M-]") 'sp-unwrap-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "M-k") 'sp-kill-hybrid-sexp)

;;Projectile
;;----------
;;package-install <RET> projectile
(projectile-mode)

(require 'ag)
;;For ag to be found on MacOS (it's installed in /usr/local/bin)
;;For ag to run
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

;;Yasnippet
;;---------
;;package-list-packages → yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/dotfiles/emacs/snippets"))
(yas-global-mode 1)

;; Markdown-mode
;; -------------
(require 'markdown-mode)

;; Company-mode
;; ------------
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Company-tern
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (company-mode)))

;; Elpy
;; ----
(require 'elpy)
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
;; Ensures that it uses the homebrew version, as set in .bashrc
(setq elpy-rpc-python-command "python2")

;; js2-mode
;; --------
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-strict-missing-semi-warning nil)

;;Misc
;;====
;;Hunspell is in /usr/local/bin, so needs to be after that setenv
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

