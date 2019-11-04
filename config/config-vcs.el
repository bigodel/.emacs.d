;;; config-vcs.el --- VCS configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; vcs variables
(setvar smerge-command-prefix "\C-cn")  ; the default value is "C-c ^"
(setvar vc-handled-backends (delq 'Git vc-handled-backends)) ; no vc for git
(setvar vc-make-backup-files t)         ; bakcup vcs files like other files
(setvar vc-follow-symlinks t)           ; automatically follow symlinks

;; i have a commit template and the first line is a comment
(add-hook 'git-commit-mode-hook
          (lambda ()
            (next-line)
            (after 'evil
              (evil-insert-state))))



;;; packages
(lazy-major-mode "\\.gitignore\\'" 'gitignore-mode)
(lazy-major-mode "\\.gitconfig\\'" 'gitconfig-mode)
(lazy-major-mode "\\.gitattributes\\'" 'gitattributes-mode)



;;; magit
(when (executable-find "git")
  (require-package 'magit))

;;; magit variables
(after 'magit
  ;; (setvar magit-section-show-child-couno t)
  (setvar magit-diff-arguments '("--histogram"))
  ;; (setvar magit-ediff-dwim-show-on-hunks t)
  (setvar magit-completing-read-function 'ivy-completing-read)
  ;; default merge arguments
  (setvar magit-merge-arguments '("--no-ff"))
  (setvar magit-auto-revert-mode nil)

  ;; TODO: understand this
  (add-hook 'magit-post-display-buffer-hook
            (lambda ()
              (when (string-match "*magit:" (buffer-name))
                (delete-other-windows))))

  (require-package 'magit-todos)
  (setvar magit-todos-fontify-org nil)
  (when (executable-find "rg")
    (setvar magit-todos-scanner 'magit-todos--scan-with-rg))
  (magit-todos-mode t))

;; TODO: move this to bindings-vcs.el
(/bindings/define-key (current-global-map)
  (kbd "C-x g") #'magit-status)

(provide 'config-vcs)
;;; config-vcs.el ends here
