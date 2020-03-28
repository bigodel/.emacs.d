;;; config-lsp.el --- lsp-mode and dap-mode configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install lsp-mode
(require-package 'lsp-mode)
;; helper packges
(after 'lsp-mode
  (require-package 'lsp-ui)
  (require-package 'dap-mode)        ; has lsp-mode and treemacs as dependencies
  ;; ivy wrapper
  (after 'ivy
    (require-package 'lsp-ivy))
  ;; treemacs wrapper
  (after 'treemacs
    (require-package 'lsp-treemacs)))

(setvar 'lsp-keymap-prefix "C-l")        ; change the default prefix for lsp
(setvar 'lsp-auto-configure t)           ; will configure company, flycheck, ...
(setvar 'lsp-log-io t)                   ; log msgs from the ls in *lsp-log*
(setvar 'lsp-session-file                ; where to store the session file
        (concat dotemacs-cache-directory ".lsp-session-v1"))
(setvar 'lsp-enable-semantic-highlighting t) ; experimental semantic highlight


;; enable which key integration
(after 'which-key
  (add-hook 'lsp-mode #'lsp-enable-which-key-integration))

;;; config-lsp.el ends here
