;;; config-yasnippet.el --- Yasnippet configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install yasnippet and the snippets
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

;;; variables
(setvar 'yas-fallback-behaviour 'return-nil)
(setvar 'yas-also-auto-indent-first-line t)
(setvar 'yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))
(after 'yasnippet-snippets
  (setvar 'yas-snippet-dirs `(,(concat user-emacs-directory "snippets")
                              ,yasnippet-snippets-dir)))

;; active yasnippet globally
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)

(provide 'config-yasnippet)
;;; config-yasnippet.el ends here
