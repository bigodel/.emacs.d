;;; config-lsp.el --- lsp-mode and dap-mode configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install lsp-mode
(require-package 'lsp-mode)
;; helper packges
(after 'lsp-mode
  (require-package 'lsp-ui)
  ;; ivy wrapper
  (after 'ivy
    (require-package 'lsp-ivy))
  ;; treemacs wrapper
  (after 'treemacs
    (require-package 'lsp-treemacs))

  ;; install debugger adapt protocol mode
  (require-package 'dap-mode))       ; has lsp-mode and treemacs as dependencies

;; change the default prefix for lsp
(setvar lsp-keymap-prefix "C-l")

;; enable which key integration
(add-hook 'lsp-mode #'lsp-enable-which-key-integration)

;;; config-lsp.el ends here
