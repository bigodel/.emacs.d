
(require-package 'flycheck)

(setq flycheck-standard-error-navigation t)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
(setq flycheck-idle-change-delay 0.8)
(setq flycheck-display-errors-function
      #'flycheck-display-error-messages-unless-error-list)

(after 'web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(after [evil flycheck]
  (evil-define-key 'normal flycheck-error-list-mode-map
    "j" #'flycheck-error-list-next-error
    "k" #'flycheck-error-list-previous-error))

(defun /flycheck/advice/next-error-find-buffer (orig-func &rest args)
  (let* ((special-buffers
          (cl-loop for buffer in (mapcar #'window-buffer (window-list))
                   when (with-current-buffer buffer
                          (and
                           (eq (get major-mode 'mode-class) 'special)
                           (boundp 'next-error-function)))
                   collect buffer))
         (first-special-buffer (car special-buffers)))
    (if first-special-buffer
        first-special-buffer
      (apply orig-func args))))

(advice-add #'next-error-find-buffer :around #'/flycheck/advice/next-error-find-buffer)

(provide 'init-flycheck)
