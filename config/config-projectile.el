;;; config-projectile.el --- Projectile configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install `projectile'
(require-package 'projectile)

;; variables
(setvar 'projectile-cache-file
        (expand-file-name "projectile.cache" dotemacs-cache-directory))
(setvar 'projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" dotemacs-cache-directory))
(setvar 'projectile-indexing-method 'alien)
(setvar 'projectile-enable-caching t)
(setvar 'projectile-completion-system 'ivy)
(setvar 'projectile-project-compilation-cmd "")
(setvar 'projectile-project-run-cmd "")

;;; start `projectile'
(projectile-mode)

;;; configuration
(after 'projectile
  ;; ignored directories
  (dolist (dir dotemacs-globally-ignored-directories)
    (add-to-list 'projectile-globally-ignored-directories dir))

  ;; projectile-generic-command
  (cond
   ((executable-find "rg")
    (setvar 'projectile-generic-command
            (concat "rg -0 --files --color never "
                    (mapconcat (lambda (dir) (concat "--glob " "'!" dir "'"))
                               projectile-globally-ignored-directories " "))))
   ((executable-find "ag")
    (setvar 'projectile-generic-command
            (concat "ag -0 -l --nocolor "
                    (mapconcat (lambda (dir) (concat "--ignore-dir=" dir))
                               projectile-globally-ignored-directories " "))))
   ((executable-find "ack")
    (setvar 'projectile-generic-command
            (concat "ack -f --print0"
                    (mapconcat (lambda (dir) (concat "--ignore-dir=" dir))
                               projectile-globally-ignored-directories " "))))))

(provide 'config-projectile)
;;; config-projectile.el ends here
