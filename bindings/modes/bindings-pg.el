;;; bindings-pg.el --- Proof General bindings -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; general proof-general bindings
(after 'proof-script
  (bindings-define-keys proof-mode-map
    ((kbd "M-n") #'proof-assert-next-command-interactive "next command")
    ((kbd "M-p") #'proof-undo-last-successful-command "undo command"))

  (after 'evil
    (evil-ex-define-cmd "pr[ove]" 'proof-goto-point)))

;;; protected undo and redo
(after [config-pg evil undo-tree]
  (evil-define-key '(normal visual motion) proof-mode-map
    "u" #'pg-undo
    (kbd "C-r") #'pg-redo))

;;; company-coq
(after 'company-coq
  (bindings-define-keys company-coq-map
    ((kbd "RET") nil)
    ((kbd "SPC") nil)))

(provide 'config-bindings-pg)
;;; bindings-pg.el ends here
