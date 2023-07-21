;; Sandbox for developing a major mode (haxe)

(add-to-list 'load-path "/home/xdl/dev/zz-haxe/haxemacs/test")

;; For the test suite
(add-to-list 'load-path "/home/xdl/dev/zz-haxe/haxemacs/src")
(defun reload-haxe-mode ()
  "Reloads haxe-mode for iterative development."
  (interactive)
  (progn
    (if (featurep 'haxe-mode)
        (unload-feature 'haxe-mode 'force))
    (if (featurep 'funda-haxe-mode)
        (unload-feature 'funda-haxe-mode 'force))
    (require 'haxe-mode)
    (revert-buffer t t)))
(use-package haxe-mode)

(defun reload-company-haxe ()
  "Reloads company-haxe for iterative development."
  (interactive)
  (progn
    (setq company-backends (remove 'company-haxe-backend company-backends))
    (if (featurep 'company-haxe)
        (unload-feature 'company-haxe 'force))
    (if (featurep 'haxe-completion)
        (unload-feature 'haxe-completion 'force))
    (require 'company-haxe)))
(use-package company-haxe)

(defun reload-haxe-eldoc ()
  "Reloads haxe-eldoc for iterative development."
  (interactive)
  (progn
    (if (featurep 'haxe-eldoc)
        (unload-feature 'haxe-eldoc 'force))
    (if (featurep 'haxe-completion)
        (unload-feature 'haxe-completion 'force))
    (require 'haxe-eldoc)))
(use-package haxe-eldoc)

(defun rerun-haxe-tests ()
  "Reloads haxemacs-completion and re-runs the test suite for it."
  (interactive)
  (progn
    (when (featurep 'run-tests)
      (unload-feature 'run-tests 'force))
    (when (featurep 'haxe-completion)
      (unload-feature 'haxe-completion 'force))
    (ert-delete-all-tests)
    (require 'run-tests)
    (ert t)))
(use-package run-tests)

(add-to-list 'load-path "/Volumes/SecondarySSD/dev/third_party/funda-haxe-mode")
(defun reload-funda-haxe-mode ()
  "Reloads funda-haxe-mode for iterative development."
  (interactive)
  (progn
    (if (featurep 'haxemacs)
        (unload-feature 'haxemacs 'force))
    (if (featurep 'funda-haxe-mode)
        (unload-feature 'funda-haxe-mode 'force))
    (require 'funda-haxe-mode)
    (revert-buffer t t)))
;; (require 'funda-haxe-mode)
