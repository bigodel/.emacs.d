
(require-package 'company)

(setq company-idle-delay 0.5)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers t)
(setq company-tooltip-limit 20)

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)

(setq company-dabbrev-code-ignore-case t)
(setq company-dabbrev-code-everywhere t)

(setq company-global-modes
      '(not
        eshell-mode comint-mode text-mode erc-mode))

(global-company-mode)

(after 'yasnippet
  (setq company-backends
        (mapcar
         (lambda (backend)
           (if (and (listp backend) (member 'company-yasnippet backend))
               backend
             (append (if (consp backend) backend (list backend))
                     '(:with company-yasnippet))))
         company-backends)))

(after 'company
  (define-key company-active-map (kbd "<backtab>") #'company-select-previous)
  (define-key company-active-map (kbd "<tab>") #'company-select-next)
  (after "yasnippet-autoloads"
    (define-key company-active-map (kbd "<tab>")
      (bind (when (null (yas-expand))
              (company-select-next))))))

(provide 'config-company)
