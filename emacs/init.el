;;Presentation
;;------------

;;to get rid of the tool and scroll bar for graphical GnuEmacs
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-15")))
;;  MacOS Sierra
;;  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-16")))

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

;;enabling sessions
;;http://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
(desktop-save-mode 1)

;;https://www.emacswiki.org/emacs/ShowParenMode
;;see the matching parens
(show-paren-mode 1)
;;remove the delay
(setq show-paren-delay 0)

;;highlight the current line
;;(turning it off because it won't make the highlighting show up)
;;(global-hl-line-mode 1)
;;(set-face-foreground 'highlight nil)
;;(set-face-underline-p 'highlight t)

;;Use vertical split by default
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
;;------

;; Evil
(require 'evil)
(evil-mode 1)
;;https://github.com/syl20bnr/evil-escape

;; Evil escape
(require 'evil-escape)
(evil-escape-mode t)
(setq-default evil-escape-key-sequence "jk")

;; linum-mode 
(require 'linum)
(global-linum-mode t)

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

;; Ido
;;use ido for buffer switching
;;http://ergoemacs.org/emacs/emacs_buffer_switching.html
(require 'ido)
(ido-mode t)

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

;https://www.emacswiki.org/emacs/ParEdit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'racket-mode-hook           #'enable-paredit-mode)
