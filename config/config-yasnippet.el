;;; config-yasnippet.el --- Yasnippet configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; install yasnippet and the snippets
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(require-package 'yasnippet-classic-snippets)

;; variables
(setvar yas-fallback-behaviour 'return-nil)
(setvar yas-also-auto-indent-first-line t)

;; active yasnippet globally
(yas-global-mode)

;; snippets folder
(yas-load-directory (expand-file-name "snippets/" user-emacs-directory))

(provide 'config-yasnippet)
;;; config-yasnippet.el ends here
