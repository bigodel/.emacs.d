;;; config-lsp.el --- lsp-mode and dap-mode configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install lsp-mode
(defun lsp-start ()
  "Start `lsp-mode' with my configurations.

This is wrapped in a function because I don't want to install
`lsp-mode' and all the other helper modes and set all the
configuration if I'm not going to use `lsp-mode'. So, in each
configuration file that I want to use lsp, instead of using the
`lsp' function, I use my `lsp-start'."
  (interactive)

  ;; install the necessary packages
  (require-package 'lsp-mode)
  (require-package 'lsp-ui)
  (require-package 'dap-mode)        ; has lsp-mode and treemacs as dependencies
  (after 'ivy
    (require-package 'lsp-ivy))         ; TODO: see the necessity of this
  (after 'treemacs
    (require-package 'lsp-treemacs))    ; TODO: see the necessity of this

  ;; variables
  (setvar 'lsp-keymap-prefix "C-l")        ; change the default prefix for lsp
  (setvar 'lsp-auto-configure t)        ; will configure company, flycheck, ...
  (when (fboundp 'flycheck-mode)
    (setvar 'lsp-prefer-flymake nil))     ; force flycheck
  (setvar 'lsp-log-io t)                   ; log msgs from the ls in *lsp-log*
  (setvar 'lsp-session-file                ; where to store the session file
          (concat dotemacs-cache-directory ".lsp-session-v1"))
  (setvar 'lsp-enable-semantic-highlighting t) ; experimental semantic highlight
  (setvar 'lsp-diagnostics-modeline-scope :project) ; modeline show project err

  ;; start lsp
  (lsp)
  (lsp-treemacs-sync-mode t))            ; TODO: investigate what is this

(after 'lsp-mode
  ;; see error statistics in modeline TODO: test this!
  (add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-modeline))

;; enable which key integration
(after 'which-key
  (add-hook 'lsp-mode #'lsp-enable-which-key-integration))

;;; config-lsp.el ends here
