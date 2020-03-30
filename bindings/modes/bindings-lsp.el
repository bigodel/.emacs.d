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
      ;; these are causing an extreme amount of lag, investigate
      ;; ([remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
      ;; ([remap xref-find-references] #'lsp-ui-peek-find-references)
      ((kbd "M-d") #'lsp-ui-doc-focus-frame))))

(after [lsp-mode evil]
  (evil-define-key '(normal visual) lsp-mode-map
    "gd" #'lsp-find-definitions
    "gr" #'lsp-rename)

  (after 'lsp-ui
    (evil-define-key '(normal visual) lsp-ui-mode-map
      "gd" #'lsp-ui-peek-find-definitions)))

(provide 'bindings-lsp)
;;; bindings-lsp.el ends here
