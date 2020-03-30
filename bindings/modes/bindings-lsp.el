;;; bindings-lsp.el --- LSP bindings

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(after 'lsp-mode
  (bindings-define-keys lsp-mode-map
    ((kbd "C-c C-d") #'lsp-describe-thing-at-point)
    ([remap xref-find-definitions] #'lsp-find-definition)
    ([remap xref-find-references] #'lsp-find-references))

  (after 'lsp-ui
    (bindings-define-keys lsp-ui-mode-map
      ([remap imenu] #'lsp-ui-imenu)
      ;; these are causing an extreme amount of lag, investigate
      ;; ([remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
      ;; ([remap xref-find-references] #'lsp-ui-peek-find-references)
      ((kbd "M-l <tab>") #'lsp-ui-doc-focus-frame) ; gui
      ((kbd "M-l TAB") #'lsp-ui-doc-focus-frame)))) ; terminal

(after [lsp-mode evil]
  (evil-define-key '(normal visual) lsp-mode-map
    "ga" #'lsp-execute-code-action
    "gR" #'lsp-rename)

  (after 'lsp-ui
    (evil-define-key '(normal visual) lsp-mode-map
      "gD" #'lsp-ui-doc-glance
      "g?" #'lsp-ui-doc-glance)

    (evil-define-key '(normal visual) lsp-ui-imenu-mode-map
      "q" #'lsp-ui-imenu--kill)

    (evil-define-key '(normal visual) lsp-ui-doc-frame-mode-map
      "q" #'lsp-ui-doc-unfocus-frame)))

(provide 'bindings-lsp)
;;; bindings-lsp.el ends here
