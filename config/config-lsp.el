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

  ;;; install
  (require-package 'lsp-mode)
  (require-package 'lsp-ui)
  (require-package 'dap-mode)        ; has lsp-mode and treemacs as dependencies
  (after 'ivy
    (require-package 'lsp-ivy))         ; TODO: see the necessity of this
  (after 'treemacs
    (require-package 'lsp-treemacs))    ; TODO: see the necessity of this

  ;;; variables
  (setvar 'lsp-keymap-prefix "M-l")        ; change the default prefix for lsp
  (setvar 'lsp-auto-configure t)        ; will configure company, flycheck, ...
  (setvar 'lsp-keep-workspace-alive nil) ; kill lsp with the last buffer
  (setvar 'lsp-diagnostic-package :auto) ; force flycheck
  (setvar 'lsp-log-io t)                   ; log msgs from the ls in *lsp-log*
  (setvar 'lsp-session-file                ; where to store the session file
          (concat dotemacs-cache-directory "lsp-session-v1"))
  (setvar 'lsp-enable-semantic-highlighting t) ; experimental semantic highlight
  (setvar 'lsp-diagnostics-modeline-scope :project) ; modeline show project err
  (setvar 'lsp-prefer-capf              ; capf if company-lsp is not installed
          (not (featurep 'company-lsp)))

  ;; this section recommends settings these two variables. i like to have
  ;; gc-cons-threshold at its default value, but whenever i'm using lsp-mode it
  ;; makes sense to bump it up to 100MB (i've tried it with the default value,
  ;; if the file has more than 2000 lines emacs becomes unusable from so many
  ;; garbage collections). and the other variable is the amount of data which
  ;; emacs reads from the process. the default (at the time of writing this) is
  ;; 4KB, but some language servers responses are 800KB to 3MB. read more on:
  ;; https://github.com/emacs-lsp/lsp-mode#performance
  ;; (setvar 'default-gc-cons-threshold 100000000)
  ;; (setvar 'gc-cons-threshold default-gc-cons-threshold)
  (setvar 'read-process-output-max (* 1024 1024)) ; 1MB

  ;;; lsp-ui configuration
  ;; peek
  (setvar 'lsp-ui-peek-enable t)           ; enable or disable peek
  (setvar 'lsp-ui-peek-show-directories t) ; show the directories of files
  ;; doc
  (setvar 'lsp-ui-doc-enable nil)          ; enable or disable doc
  (setvar 'lsp-ui-doc-max-width 80)        ; max width of the doc pop-up
  (setvar 'lsp-ui-doc-use-webkit nil)      ; use emacs native pop ups
  (setvar 'lsp-ui-doc-include-signature t) ; include object signature
  (setvar 'lsp-ui-doc-position 'at-point)  ; position of the doc pop up
  ;; sideline
  (setvar 'lsp-ui-sideline-enable nil)  ; enable or disable sideline
  (setvar 'lsp-ui-sideline-delay 0.5)   ; how many secs to wait before showing
  (setvar 'lsp-ui-sideline-show-code-actions nil) ; don't show code actions
  (setvar 'lsp-ui-sideline-show-hover t)          ; show hover messages
  (setvar 'lsp-ui-sideline-show-diagnostics t)    ; show diagnostics messages
  ;; imenu
  (setvar 'lsp-ui-imenu-enable t)       ; enable or disable imenu

  ;;; clients configuration

  ;;; start lsp
  (lsp)
  (lsp-treemacs-sync-mode t))            ; TODO: investigate what is this

(defun lsp-suggest-project-root ()
  "Suggest the nearest project root.
This overrides `lsp--suggest-project-root'."
  (or
   (locate-dominating-file
    (buffer-file-name)
    (lambda (dir)
      (if (string-match-p "node_modules" dir)
          nil
        (file-exists-p (concat dir "package.json")))))
   (when (featurep 'projectile) (condition-case nil
                                    (projectile-project-root)
                                  (error nil)))
   (when (featurep 'project)
     (when-let ((project (project-current)))
       (car (project-roots project))))))

(after 'lsp-mode
  ;; override the default `lsp--suggest-project-root' with our function
  (advice-add #'lsp--suggest-project-root :override #'lsp-suggest-project-root)
  ;; see error statistics in modeline TODO: test this!
  (add-hook 'lsp-managed-mode-hook #'lsp-diagnostics-modeline-mode)

  ;; enable which key integration
  (after 'which-key
    (add-hook 'lsp-mode #'lsp-enable-which-key-integration)))

(provide 'config-lsp)
;;; config-lsp.el ends here
