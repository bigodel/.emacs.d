;;; config-flycheck.el --- Flycheck configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; install flycheck
(require-package 'flycheck)

(setvar 'flycheck-display-errors-function ; only display errors if the error
        #'flycheck-display-error-messages-unless-error-list) ; list is not on

;; not really sure what this does
;; (after "web-mode-autoloads"
;;   (flycheck-add-mode 'javascript-eslint 'web-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; make emacs display the erros similar to IDE's
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))

(provide 'config-flycheck)
;;; config-flycheck.el ends here
