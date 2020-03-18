;;; config-lsp.el --- lsp-mode configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; packages to help with lsp
;; (require-package ...)
;; install lsp-mode
(require-package 'lsp-mode)

;; change the default prefix for lsp
(setvar lsp-keymap-prefix "C-l")

;; enable which key integration
(add-hook 'lsp-mode #'lsp-enable-which-key-integration)

;;; config-lsp.el ends here
