;;; bindings-pg.el --- Proof General bindings

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(after 'proof-script
  (bindings-define-keys proof-mode-map
    ((kbd "M-n") #'proof-assert-next-command-interactive "next command")
    ((kbd "M-p") #'proof-undo-last-successful-command "undo command"))

  (after 'evil
    (evil-ex-define-cmd "pr[ove]" 'proof-goto-point)))

(provide 'bindings-pg)
;;; bindings-pg.el ends here
