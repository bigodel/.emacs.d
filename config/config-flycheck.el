;;; config-flycheck.el --- Flycheck configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install `flycheck'
(require-package 'flycheck)

(setvar 'flycheck-keymap-prefix (kbd "C-c f")) ; change flyspell prefix
(setvar 'flycheck-display-errors-function ; only display errors if the error
        #'flycheck-display-error-messages-unless-error-list) ; list is not on
(setvar 'flycheck-indication-mode 'right-fringe) ; where to show the error

;;; start `flycheck' globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; show flycheck errors on pop ups
(require-package 'flycheck-pos-tip)
(setvar 'flycheck-pos-tip-timeout -1)
(setvar 'flycheck-pos-tip-max-width 100)
(add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)

;; make emacs display the erros similar to IDE's
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))

;; use SPC c f for flycheck (this should be on its own file, but since its just
;; this one why not leave it here)
(after [config-bindings evil flycheck]
  (bindings-define-key bindings-space-map
    "cf" flycheck-command-map))

(provide 'config-flycheck)
;;; config-flycheck.el ends here
