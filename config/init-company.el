
(require-package 'company)
;; (require 'company)

(setq company-idle-delay nil)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-limit 20)
(setq company-auto-complete 'company-explicit-action-p)

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

(provide 'init-company)
