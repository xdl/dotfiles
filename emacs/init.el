; use shift + arrow keys to move between frames
(windmove-default-keybindings)

;;(defvar scheme-program-name "guile")

;;this one looks more recommended:
;;http://www-users.cs.umn.edu/~gini/1901-07s/emacs_scheme/
(set-variable (quote scheme-program-name) "guile")

;;to get rid of the menu and scroll bar for graphical GnuEmacs
(when (window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(load-theme 'wombat)

;;usable light color scheme
;;(load-theme 'whiteboard)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
      t)
  (package-initialize))

;expand selection
;(require 'expand-region)
;(global-set-key (kbd "C-=") 'er/expand-region)

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

;;https://www.emacswiki.org/emacs/ParEdit
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/orgs/calendar.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
