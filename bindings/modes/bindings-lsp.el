;;; bindings-lsp.el --- LSP bindings -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(after 'lsp-mode
  (bindings-define-keys lsp-mode-map
    ((kbd "C-c C-d") #'lsp-describe-thing-at-point)
    ([remap xref-find-definitions] #'lsp-find-definition)
    ([remap xref-find-references] #'lsp-find-references))

  ;; for some reason this needs to be defined here and can't be define in
  ;; bindings-hydra.el like i intended
  (after [hydra lsp-ui]
    (defhydra hydras/lsp/workspace (:exit t)
      ("a" lsp-workspace-folders-add "add folder" :column "workspace")
      ("r" lsp-workspace-folders-remove "remove folder")
      ("s" lsp-workspace-folders-switch "switch folder"))

    (defhydra hydras/lsp (:exit t)
      ("d" lsp-ui-peek-find-definitions "peek definition" :column "definitions")
      ("D" xref-find-definitions "xref definitions")

      ("r" lsp-ui-peek-find-references "peek references" :column "references")
      ("R" xref-find-references "xref references")
      ("u" lsp-treemacs-references "usages")

      ("n" lsp-rename "rename" :column "refactor")
      ("=" lsp-format-buffer "format")
      ("a" lsp-ui-sideline-apply-code-actions "apply code action")

      ("s" lsp-treemacs-symbols "symbols" :column "info")

      ("e" lsp-treemacs-errors-list "list" :column "errors")

      ("S" lsp-restart-workspace "restart workspace" :column "workspace")
      ("w" hydras/lsp/workspace/body "folders")
      ("i" lsp-describe-session "session info"))

    (bindings-define-keys lsp-ui-mode-map
      ([remap imenu] #'lsp-ui-imenu)
      ;; these are causing an extreme amount of lag, investigate
      ;; ([remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
      ;; ([remap xref-find-references] #'lsp-ui-peek-find-references)
      ((kbd "M-l <tab>") #'lsp-ui-doc-focus-frame) ; gui
      ((kbd "M-l tab") #'lsp-ui-doc-focus-frame) ; gui
      ((kbd "M-l TAB") #'lsp-ui-doc-focus-frame))) ; terminal

  (after 'lsp-treemacs
    (bindings-define-keys lsp-mode-map
      ([remap flycheck-list-errors] #'lsp-treemacs-errors-list)
      ([remap lsp-find-references] #'lsp-treemacs-references)))

  (after 'evil
    (evil-define-key '(normal visual) lsp-mode-map
      "K" #'lsp-describe-thing-at-point
      "ga" #'lsp-execute-code-action
      "gr" #'lsp-rename)

    ;; TODO: add lsp-ui-peek-find-definitions
    (after 'lsp-ui
      (evil-define-key '(normal visual) lsp-mode-map
        (kbd "RET") #'hydras/lsp/body
        (kbd "<ret>") #'hydras/lsp/body
        "gD" #'lsp-ui-doc-glance
        "g?" #'lsp-ui-doc-glance)

      (evil-define-key '(normal visual) lsp-ui-imenu-mode-map
        "q" #'lsp-ui-imenu--kill)

      (evil-define-key '(normal visual) lsp-ui-doc-frame-mode-map
        "q" #'lsp-ui-doc-unfocus-frame))))

(provide 'config-bindings-lsp)
;;; bindings-lsp.el ends here
