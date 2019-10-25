;;; config-projectile.el --- Projectile configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(require-package 'projectile)

;; variables
(setvar projectile-cache-file
        (concat dotemacs-cache-directory "projectile.cache"))
(setvar projectile-known-projects-file
        (concat dotemacs-cache-directory "projectile-bookmarks.eld"))
(setvar projectile-indexing-method 'native)
(setvar projectile-enable-caching t)
(setvar projectile-completion-system 'ivy)
(setvar projectile-project-compilation-cmd "")
(setvar projectile-project-run-cmd "")

;; default prefix key for projectile
(/bindings/define-key (current-global-map) (kbd "C-c p") #'projectile-command-map)

;; start projectile
(projectile-mode t)

;; add ignored directories to projectile
(dolist (dir dotemacs-globally-ignored-directories)
  (add-to-list 'projectile-globally-ignored-directories dir))

(cond
 ((executable-find "ag")
  (setvar projectile-generic-command
          (concat "ag -0 -l --nocolor"
                  (mapconcat #'identity
                             (cons "" projectile-globally-ignored-directories)
                             " --ignore-dir="))))
 ((executable-find "ack")
  (setvar projectile-generic-command
          (concat "ack -f --print0"
                  (mapconcat #'identity
                             (cons "" projectile-globally-ignored-directories)
                             " --ignore-dir=")))))

(provide 'config-projectile)
;;; config-projectile.el ends here
