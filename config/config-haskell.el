;;; config-haskell.el --- Haskell configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install `haskell-mode' only if we visit a dart file
(lazy-major-mode "\\.hs\\'" 'haskell-mode)

;;; `haskell-mode' configuration
(after 'haskell-mode
  ;;; `lsp-haskell'
  ;; (if (executable-find "hie")
  ;;     (progn (require-package 'lsp-haskell)

  ;;            (after 'config-lsp
  ;;              (add-hook 'haskell-mode-hook #'lsp-init)))
  ;;   (message "Please, install `hie' to use haskell's language server."))
  )

(provide 'config-haskell)
;;; config-haskell.el ends here
