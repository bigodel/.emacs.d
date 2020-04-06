;;; bindings-pg.el --- Org mode bindings -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(after 'org
  (bindings-define-keys (current-global-map)
    ((kbd "C-c o c") #'org-capture)
    ((kbd "C-c o a") #'org-agenda)
    ((kbd "C-c o l") #'org-store-link)
    )

  (after 'evil
    (evil-define-key '(normal visual motion) 'global
      "gC" #'org-capture
      "ga" #'org-agenda
      "goc" #'org-capture
      "goa" #'org-agenda
      "gol" #'org-store-link)

    (evil-define-key '(normal visual) org-mode-map
      "gt" #'org-todo)))

(provide 'config-bindings-org)
;;; bindings-org.el ends here
