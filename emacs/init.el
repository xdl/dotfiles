;;Presentation
;;------------

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

;;usable light color scheme
;;(load-theme 'whiteboard)

;;Bindings
;;--------

;; use shift + arrow keys to move between frames
(windmove-default-keybindings)

;;Behaviour
;;---------

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

;;Lisp Development
;;----------------

;; Racket filetype detection
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;;(defvar scheme-program-name "guile")

;;this one looks more recommended:
;;http://www-users.cs.umn.edu/~gini/1901-07s/emacs_scheme/
(set-variable (quote scheme-program-name) "racket")

;;Plugins
;;-------
;;Install these with e.g. M-x package-install RET evil RET

;; Evil
;; https://github.com/noctuid/evil-guide#switching-between-evil-and-emacs
;; Disable overriding some useful emacs bindings in insert mode. Needs to be defined before loading Evil
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

;;https://github.com/syl20bnr/evil-escape
;; Evil escape
(require 'evil-escape)
(evil-escape-mode t)
(setq-default evil-escape-key-sequence "jk")
(setq evil-escape-excluded-major-modes
      '(comint-mode
	sldb-mode)) ;;Make sure this syncs up with any evil-set-initial-state calls

;; Relative numbers
(require 'nlinum-relative)
(nlinum-relative-setup-evil)                    ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(add-hook 'text-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)      ;; delay
(setq nlinum-relative-current-symbol "")      ;; or "" for display current line number
(setq nlinum-relative-offset 0)                 ;; 1 if you want 0, 2, 3...

;;Magit
(require 'magit)
;;https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-set-key (kbd "C-x g") 'magit-status)

;;Rainbow Delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;Expand Selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Helm
(require 'helm-config)
(helm-mode 1)
;; Using Helm command completion instead of default
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h a") 'helm-apropos) ;;overriding apropos command
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (setq helm-ff-skip-boring-files t)
;; (setq helm-boring-file-regexp-list
;;       '("node_modules$"))
      
;; Ido (trying out Helm since it's got fuzzy finding)
;;use ido for buffer switching
;;http://ergoemacs.org/emacs/emacs_buffer_switching.html
;;(require 'ido)
;;(ido-mode t)

;;Orgmode
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
   (sh . t)
   (ruby . t)
   (scheme .t)))

;;disable confirmation prompt for languages that don't tend to touch files
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "emacs-lisp")))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Geiser
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

;; Smartparens

(require 'smartparens-config)
(smartparens-global-mode t)

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

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

;; Projectile
(projectile-mode)
