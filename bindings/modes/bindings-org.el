;;; bindings-pg.el --- Org mode bindings -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(bindings-define-keys (current-global-map)
  ((kbd "C-c o c") #'org-capture)
  ((kbd "C-c o a") #'org-agenda)
  ((kbd "C-c o o") #'org-agenda-list)
  ((kbd "C-c o l") #'org-store-link)
  ((kbd "C-c o t") #'org-todo-list)
  ((kbd "C-c o i") (bind () (find-file org-inbox-file)))
  ((kbd "C-c o n") (bind () (find-file org-default-notes-file)))
  ((kbd "C-c o p") (bind () (find-file (concat org-directory "/personal.org"))))
  ((kbd "C-c o u") (bind () (find-file (concat org-directory "/uni.org"))))
  ((kbd "C-c o w") (bind () (find-file (concat org-directory "/work.org")))))

(after 'evil
  (evil-define-key '(normal visual motion) 'global
    "goa" #'org-agenda
    "goo" #'org-agenda-list
    "goc" #'org-capture
    "gol" #'org-store-link
    "got" #'org-todo-list
    "goi" (bind () (find-file org-inbox-file))
    "gon" (bind () (find-file org-default-notes-file))
    "gop" (bind () (find-file (concat org-directory "/personal.org")))
    "gou" (bind () (find-file (concat org-directory "/uni.org")))
    "gow" (bind () (find-file (concat org-directory "/work.org"))))

  (evil-define-key '(normal visual) org-mode-map
    "gt" #'org-todo))

(provide 'config-bindings-org)
;;; bindings-org.el ends here
