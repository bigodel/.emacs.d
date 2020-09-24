;;; config-ts.el --- TypeScript configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install `typescript-mode' for TypeScript files
(lazy-major-mode "\\.ts\\'" 'typescript-mode)

;;; deal with TSX files with `web-mode'
(after 'web-mode
  ;; i associate TSX files with `typescript-tsx-mode' derived from `web-mode'
  ;; because `typescript-mode' does not officially support TSX. see
  ;; https://github.com/emacs-typescript/typescript.el/issues/4
  (define-derived-mode typescript-tsx-mode web-mode "TSX")
  (add-to-list 'auto-mode-alist
               '("\\.tsx\\'" . (lambda ()
                                 (require-package 'web-mode)
                                 (require 'web-mode)
                                 (typescript-tsx-mode))))

  ;; add the newly created mode to the possible linters for flycheck
  (after 'flycheck
    (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)))

(after 'config-lsp
  (add-hook 'typescript-mode-hook #'lsp-init)
  (add-hook 'typescript-tsx-mode-hook #'lsp-init))

(provide 'config-ts)
;;; config-ts.el ends here
