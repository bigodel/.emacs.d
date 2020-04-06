;;; config-projectile.el --- Projectile configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(require-package 'projectile)

;; variables
(setvar 'projectile-cache-file
        (concat dotemacs-cache-directory "projectile.cache"))
(setvar 'projectile-known-projects-file
        (concat dotemacs-cache-directory "projectile-bookmarks.eld"))
(setvar 'projectile-indexing-method 'alien)
(setvar 'projectile-enable-caching t)
(setvar 'projectile-completion-system 'ivy)
(setvar 'projectile-project-compilation-cmd "")
(setvar 'projectile-project-run-cmd "")

;; start projectile
(projectile-mode)

(after 'projectile
  (dolist (dir dotemacs-globally-ignored-directories)
    (add-to-list 'projectile-globally-ignored-directories dir))

  ;;; projectile-generic-command
  (cond
   ((executable-find "rg")
    (setq projectile-generic-command
          (concat "rg -0 --files --color never "
                  (mapconcat (lambda (dir) (concat "--glob " "'!" dir "'"))
                             projectile-globally-ignored-directories " "))))
   ((executable-find "ag")
    (setq projectile-generic-command
          (concat "ag -0 -l --nocolor "
                  (mapconcat (lambda (dir) (concat "--ignore-dir=" dir))
                             projectile-globally-ignored-directories " "))))
   ((executable-find "ack")
    (setq projectile-generic-command
          (concat "ack -f --print0"
                  (mapconcat (lambda (dir) (concat "--ignore-dir=" dir))
                             projectile-globally-ignored-directories " "))))))
