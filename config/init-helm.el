(require-package 'helm)

(setq helm-bookmark-show-location t)
(setq helm-buffer-max-length 40)
(setq helm-split-window-inside-p t)
(setq helm-mode-fuzzy-match t)
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-ff-skip-boring-files t)
(setq helm-follow-mode-persistent t)

(after 'helm-source
  (defun /helm/make-source (f &rest args)
    (let ((source-type (cadr args))
          (props (cddr args)))
      (unless (child-of-class-p source-type 'helm-source-async)
        (plist-put props :fuzzy-match t))
      (apply f args)))
  (advice-add 'helm-make-source :around '/helm/make-source))

(after 'helm
  (require-package 'helm-descbinds)

  (require-package 'helm-flx)
  (helm-flx-mode t)

  (require-package 'helm-dash)
  (setq helm-dash-browser-func 'eww)

  (require-package 'helm-ag)
  (setq helm-ag-fuzzy-match t)
  (setq helm-ag-use-agignore t)
  (setq helm-ag-ignore-patterns dotemacs-globally-ignored-directories)
  (after 'helm-ag
    (cond ((executable-find "ag")
           t)
          ((executable-find "pt")
           (setq helm-ag-base-command "pt -e --nogroup --nocolor"))
          ((executable-find "ack")
           (setq helm-ag-base-command "ack --nogroup --nocolor"))))

  (setq helm-swoop-pre-input-function #'ignore)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-use-fuzzy-match t)
  (require-package 'helm-swoop)

  (after "projectile-autoloads"
    (require-package 'helm-projectile))

  (require-package 'helm-tramp)

  ;; take between 10-30% of screen space
  (setq helm-autoresize-min-height 10)
  (setq helm-autoresize-max-height 30)
  (helm-autoresize-mode t))

(/boot/delayed-init
 (progn
   (global-set-key [remap execute-extended-command] #'helm-M-x)
   (global-set-key [remap find-file] #'helm-find-files)
   (helm-mode t)))

(provide 'init-helm)
