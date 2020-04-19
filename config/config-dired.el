;;; config-dired.el --- Dired configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; wdired
(setvar 'wdired-allow-to-change-permissions t) ; edit permissions
(setvar 'wdired-allow-to-redirect-links t)  ; edit target of symlinks
(setvar 'wdired-use-interactive-rename nil) ; don't require confirmation
(setvar 'wdired-confirm-overwrite t)        ; but confirm on overwrite
;; dired-x is a library to add extra functionality to dired, for more info refer
;; to the GNU manual
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/
(add-hook 'dired-load-hook
          (lambda ()
            "Load `dired-x' and `dired-aux' when `dired' first loads.
Set `dired-x' and `dired-aux' global variables here."
            (require 'dired-x)
            (require 'dired-aux)
            ;; Set dired-x and dired-aux global variables here. For example:
            ;; (setvar 'dired-guess-shell-gnutar "gtar")
            ;; (setvar 'dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            "Activate `hl-line-mode' when `dired' starts. Set
`dired-x' and `dired-aux' buffer-local variables here."
            ;; Set dired-x and dired-aux buffer-local variables here.
            ;; For example:
            ;; (dired-omit-mode 1)
            (hl-line-mode)
            ))

;; *BSD's 'ls' command does not support the "--dired" option needed by Emacs
;; alternatively, we check if 'gls' is installed, which is GNU's ls, and if not
;; we use Emacs's own emulation of 'ls'
(when (string= system-type "berkeley-unix")
  (if (executable-find "gls")
      (setvar 'insert-directory-program (executable-find "gls"))
    (setvar 'dired-use-ls-dired nil)
    (setvar 'ls-lisp-use-insert-directory-program nil)))

;; dired-subtree configuration
(after 'dired
  (require-package 'dired-subtree))

(provide 'config-dired)
;;; config-dired.el ends here
