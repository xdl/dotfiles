; use shift + arrow keys to move between frames
(windmove-default-keybindings)

;;(defvar scheme-program-name "guile")

;;this one looks more recommended:
;;http://www-users.cs.umn.edu/~gini/1901-07s/emacs_scheme/
(set-variable (quote scheme-program-name) "racket")

;;to get rid of the tool and scroll bar for graphical GnuEmacs
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-15")))
;;  MacOS Sierra
;;  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline-16")))
;; get rid of menu bar regardless
(menu-bar-mode -1)

(load-theme 'wombat)

;;usable light color scheme
;;(load-theme 'whiteboard)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
      t)
  (package-initialize))

;; Slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;expand selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;enabling sessions
;;http://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
(desktop-save-mode 1)

;;see the matching parens
(show-paren-mode 1)

;;remove the delay
(setq show-paren-delay 0)

;;highlight the current line
;;(turning it off because it won't make the highlighting show up)
;;(global-hl-line-mode 1)
;;(set-face-foreground 'highlight nil)
;;(set-face-underline-p 'highlight t)

;;use ido for buffer switching
;;http://ergoemacs.org/emacs/emacs_buffer_switching.html
(require 'ido)
(ido-mode t)

;;orgmode
;;from David O'Toole's tutorial: http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;;workflows:
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;; Allowing certain languages to be evaluated in code blocks: http://orgmode.org/manual/Languages.html#Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (js . t)
   (python . t)
   (ruby . t)
   (scheme .t)))

;; Scheme integration

; For MacOS
; http://www.nongnu.org/geiser/geiser_3.html#Customization-and-tips
(setq geiser-racket-binary "/usr/local/bin/racket")

;; Racket filetype detection
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;https://www.emacswiki.org/emacs/ParEdit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;save custom variables elsewhere:
;http://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp
(let ((my-custom-file "~/.emacs-custom.el"))
  (unless (file-exists-p my-custom-file)
    (write-region "" nil my-custom-file))
  ;;set and load it
  (setq custom-file my-custom-file)
  (load custom-file))

;; stop that annoying beeping https://www.emacswiki.org/emacs/AlarmBell#toc3
(setq ring-bell-function 'ignore)

(require 'magit)

;;https://magit.vc/manual/magit/Getting-started.html#Getting-started
(global-set-key (kbd "C-x g") 'magit-status)
