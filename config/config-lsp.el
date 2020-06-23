;;; config-lsp.el --- LSP configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(require-package 'dap-mode)        ; has lsp-mode and treemacs as dependencies
(after 'ivy
  (require-package 'lsp-ivy))         ; TODO: see the necessity of this
(after 'treemacs
  (require-package 'lsp-treemacs)     ; TODO: see the necessity of this
  (lsp-treemacs-sync-mode t))         ; sync lsp and treemacs workspaces

;;; lsp variables
(setvar 'lsp-keymap-prefix "C-c l")      ; change the default prefix for lsp
(setvar 'lsp-idle-delay 0.5)           ; how ofter lsp refreshes
(setvar 'lsp-enable-symbol-highlighting nil) ; dont highlight symbol at ponint
(setvar 'lsp-auto-configure t)        ; will configure company, flycheck, ...
(setvar 'lsp-keep-workspace-alive nil) ; kill lsp with the last buffer
(setvar 'lsp-diagnostic-package :auto) ; package to use for findings errors
(setvar 'lsp-enable-semantic-highlighting t) ; experimental semantic highlight
(setvar 'lsp-enable-snippet t)               ; enable snippet completion
(setvar 'lsp-signature-auto-activate nil)    ; don't auto activate signature
(setvar 'lsp-log-io nil)                     ; log msgs from the ls in *lsp-log*
(setvar 'lsp-print-performance nil)          ; print performance information
(setvar 'lsp-session-file                ; where to store the session file
        (expand-file-name "lsp-session-v1" dotemacs-cache-directory))
(setvar 'lsp-diagnostics-modeline-scope :project) ; modeline show project err
(setvar 'lsp-prefer-capf t)             ; whether or not to prefer capf
(setvar 'lsp-eldoc-enable-hover nil)    ; i'd rather see doc on a popup

;; the performance section recommends settings this variable. it is the amount
;; of data which emacs reads from processes. the default (at the time of
;; writing this) is 4KB, but some language servers responses are 800KB to 3MB.
;; read more on: https://github.com/emacs-lsp/lsp-mode#performance
(setvar 'read-process-output-max (* 3 1024 1024)) ; 3MB

;;; lsp-ui configuration
;; peek
(setvar 'lsp-ui-peek-enable t)           ; enable or disable peek
(setvar 'lsp-ui-peek-show-directories t) ; show the directories of files
;; doc
(setvar 'lsp-ui-doc-enable t)            ; enable or disable doc
(setvar 'lsp-ui-doc-delay 0.5)           ; delay to wait and show doc
(setvar 'lsp-ui-doc-max-width 80)        ; max width of the doc pop-up
(setvar 'lsp-ui-doc-use-webkit t)        ; use emacs native pop ups
(setvar 'lsp-ui-doc-include-signature t) ; include object signature
(setvar 'lsp-ui-doc-position 'top)       ; position of the doc pop up
(setvar 'lsp-ui-doc-header t)            ; display the symbol string
;; sideline
(setvar 'lsp-ui-sideline-enable t)      ; enable or disable sideline
(setvar 'lsp-ui-sideline-delay 0.5)     ; how many secs to wait before showing
(setvar 'lsp-ui-sideline-show-code-actions nil) ; don't show code actions
(setvar 'lsp-ui-sideline-show-hover t)          ; show hover messages
(setvar 'lsp-ui-sideline-show-diagnostics t)    ; show diagnostics messages
(setvar 'lsp-ui-sideline-ignore-duplicate t)    ; ignore duplicates
;; imenu
(setvar 'lsp-ui-imenu-enable t)       ; enable or disable imenu

;;; clients configuration

;;; hooks
;; see error statistics in modeline
(add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-modeline-mode)
;; format and organize imports on save
;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
;; (add-hook 'before-save-hook #'lsp-organize-imports t t)

(after 'lsp-ui-doc
  (add-hook 'lsp-ui-doc-frame-mode-hook
            (lambda ()
              "Disable 'lines-tail and 'empty for
`whitespace-mode' in `lsp-ui-doc-frame-mode'."
              (setvar 'whitespace-line-column nil 'local)
              (setvar 'whitespace-style
                      (remove 'lines-tail whitespace-style) 'local)
              (setvar 'whitespace-style
                      (remove 'empty whitespace-style) 'local)
              (whitespace-mode -1)
              (whitespace-mode t))))

;; enable which key integration
(after 'which-key
  (add-hook 'lsp-mode #'lsp-enable-which-key-integration))

(after 'lsp-ui
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(provide 'config-lsp)
;;; config-lsp.el ends here
