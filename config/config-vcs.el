;;; config-vcs.el --- VCS configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; vcs variables
(setvar 'smerge-command-prefix "\C-cn")  ; the default value is "C-c ^"
(setvar 'vc-make-backup-files t)         ; bakcup vcs files like other files
(setvar 'vc-follow-symlinks t)           ; automatically follow symlinks

;; i have a commit template and the first line is a comment
(add-hook 'git-commit-mode-hook
          (lambda ()
            (next-line)
            (after 'evil
              (evil-insert-state))
            (setvar 'fill-column 72 'local)))

;;; packages
(lazy-major-mode "\\.gitignore\\'" gitignore-mode)
(lazy-major-mode "\\.gitconfig\\'" gitconfig-mode)
(lazy-major-mode "\\.gitattributes\\'" gitattributes-mode)

;;; magit
(when (executable-find "git")
  (require-package 'magit)
  (require-package 'git-gutter)
  (global-git-gutter-mode))             ; TODO: add bindings for git-gutter

;;; magit variables
(after 'magit
  (setvar 'vc-handled-backends (delq 'Git vc-handled-backends)) ; no vc for git
  (setvar 'magit-diff-arguments '("--histogram"))
  (setvar 'magit-completing-read-function 'ivy-completing-read)
  (setvar 'magit-auto-revert-mode t)
  ;; this is set in my git configuration but because it uses a variable magit
  ;; isn't able to use it (although it should), so I just set it here manually
  (setvar 'magit-credential-cache-daemon-socket ; location of credential socket
          (if (getenv "XDG_CACHE_HOME")
              (expand-file-name "git/credential/socket"
                                (getenv "XDG_CACHE_HOME"))
            (expand-file-name ".cache/git/credential/socket" (getenv "HOME"))))

  ;; (require-package 'forge)              ; use forge to access pr's and issues

  (require-package 'magit-todos)
  (setvar 'magit-todos-fontify-org nil)
  (when (executable-find "rg")
    (setvar 'magit-todos-scanner 'magit-todos--scan-with-rg))
  (magit-todos-mode t))

(provide 'config-vcs)
;;; config-vcs.el ends here
