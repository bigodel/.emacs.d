;;; config-vcs.el --- VCS configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(setvar smerge-command-prefix "\C-cn")  ; the default value is "C-c ^"
(setvar vc-make-backup-files t)         ; bakcup vcs files like other files
(setvar auto-revert-check-vc-info t)    ; update vcs info when needed
(setvar vc-follow-symlinks t)

;; i have a commit template and the first line is a comment
(after 'evil
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (next-line)
              (when (evil-normal-state-p)
                (evil-insert-state)))))

(require-package 'magit)

(/boot/lazy-major-mode "\\.gitignore$" gitignore-mode)
(/boot/lazy-major-mode "\\.gitattributes$" gitattributes-mode)

;; magit variables
(after 'magit
  (setvar magit-section-show-child-couno t)
  (setvar magit-diff-arguments '("--histogram"))
  (setvar magit-ediff-dwim-show-on-hunks t)
  (setvar magit-display-buffer-function 'magit-display-buffer-traditional)
  (setvar magit-completing-read-function 'ivy-completing-read)

  (add-hook 'magit-mode-hook #'hl-line-mode)

  ;; default merge arguments
  (setvar magit-merge-arguments '("--no-ff"))

  (require-package 'magit-todos)
  (setvar magit-todos-fontify-org nil)
  (when (executable-find "rg")
    (setvar magit-todos-scanner 'magit-todos--scan-with-rg))
  (magit-todos-mode t))

;; bindings
(/bindings/define-key (current-global-map)
  (kbd "C-x g") #'magit-status)

(provide 'config-vcs)
;;; config-vsc.el ends here
