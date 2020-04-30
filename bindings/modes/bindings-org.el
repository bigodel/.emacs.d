;;; bindings-pg.el --- Org mode bindings -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(setvar 'org-global-map (make-sparse-keymap))

(bindings-define-prefix-keys org-global-map "C-c o"
  ("c" #'org-capture)
  ("a" #'org-agenda)
  ("o" #'org-agenda-list)
  ("l" #'org-store-link)
  ("t" #'org-todo-list)
  ("b" #'org-switchb)
  ("i" (bind () (find-file
                 (if (boundp 'org-inbox-file)
                     org-inbox-file
                   (expand-file-name "inbox.org" org-directory))) "inbox"))
  ("n" (bind () (find-file
                 (if (boundp 'org-notes-file)
                     org-notes-file
                   (expand-file-name "notes.org" org-directory))) "notes"))
  ("p" (bind () (find-file
                 (expand-file-name "personal.org" org-directory))) "personal")
  ("u" (bind () (find-file
                 (expand-file-name "uni.org" org-directory))) "uni")
  ("w" (bind () (find-file
                 (expand-file-name "work.org" org-directory))) "work"))

(bindings-define-key (current-global-map)
  (kbd "C-c o") org-global-map)

(after 'evil
  (evil-define-key '(normal visual motion) 'global
    "go" org-global-map)

  (evil-define-key '(normal visual) org-mode-map
    "gt" #'org-todo)

  ;; use `evil-org-return' everywhere (i use this instead of return in the
  ;; keythemes so that i can use it even on normal mode)
  (after 'evil-org
    (bindings-define-keys org-mode-map
      ((kbd "RET") #'evil-org-return)
      ((kbd "<return>") #'evil-org-return))))

(provide 'config-bindings-org)
;;; bindings-org.el ends here
