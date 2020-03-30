;;; config-yasnippet.el --- Yasnippet configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install yasnippet and the snippets
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

;;; variables
(setvar 'yas-fallback-behaviour 'return-nil)
(setvar 'yas-also-auto-indent-first-line t)
(setvar 'yas-snippet-dirs `(yasnippet-snippets-dir ; where to look for snippets
                            ,(concat user-emacs-directory "snippets")))

;; active yasnippet globally
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)

(provide 'config-yasnippet)
;;; config-yasnippet.el ends here
