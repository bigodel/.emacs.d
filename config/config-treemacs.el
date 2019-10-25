;;; config-treemacs.el --- Treemacs configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(require-package 'treemacs)

(after 'treemacs
  (treemacs-follow-mode t)                ; move focus to the current file
  ;; (treemacs-tag-follow-mode t)            ; move focus to the current tag
  (treemacs-filewatch-mode t)             ; watch for changes and refresh
  (treemacs-fringe-indicator-mode nil)    ; disable the fringe indicator
  ;; if python3 is installed, use the extended treemacs git mode, otherwise use
  ;; the deferred one
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

;; variables
(setvar treemacs-no-png-images nil)

;; bindings
(/bindings/define-keys (current-global-map)
  ((kbd "C-x t t") #'treemacs)
  ((kbd "C-x t 0") #'treemacs-select-window)
  ((kbd "C-x t 1") #'treemacs-delete-other-windows)
  ((kbd "C-x t C-t") #'treemacs-find-file)
  ((kbd "C-x t M-t") #'treemacs-find-tag))

(after 'treemacs
  (/bindings/define-keys treemacs-mode-map
    ((kbd "C-w C-w") #'other-window)
    ((kbd "C-w W") #'other-window)
    ((kbd "C-w w") #'other-window)
    ((kbd "C-w q") #'delete-window)
    ((kbd "C-w C-c") #'delete-window)
    ((kbd "j") #'treemacs-next-line)
    ((kbd "k") #'treemacs-previous-line))

  (after 'evil
    (/bindings/define-keys treemacs-mode-map
      ((kbd "C-f") #'evil-scroll-page-down)
      ((kbd "C-b") #'evil-scroll-page-up)
      ((kbd "C-u") #'evil-scroll-up)
      ((kbd "C-d") #'evil-scroll-down)
      ((kbd "C-w l") #'evil-window-right))))

(after 'projectile
  (require-package 'treemacs-projectile)
  (/bindings/define-key projectile-command-map
    (kbd "h") #'treemacs-projectile))

(provide 'config-treemacs)
;;; config-treemacs.el ends here
