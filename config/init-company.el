
(require-package 'company)

;; control what TAB does when pressed
(setq tab-always-indent 'complete)

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

(defvar completion-at-point-functions-saved nil)

(defun /company/indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(defun /company/complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

(define-key company-mode-map [remap indent-for-tab-command]
  'company-indent-for-tab-command)

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
