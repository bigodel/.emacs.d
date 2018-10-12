
(after 'org
  (setq org-directory (concat (getenv "HOME") "/docs/org"))

  (unless (file-directory-p org-directory)
    (unless (/boot/create-non-existent-directory org-directory)
    (let ((default-org (concat (getenv "HOME") "/Documents/Org")))
      (if (y-or-n-p
       (format "Failed to create `%s', use the default directory for org files [%s]?"
               org-directory default-org))
          (progn (make-directory (concat (getenv "HOME") "/Documents/Org") t)
                 (setq org-directory (concat (getenv "HOME") "/Documents")))
        (error (concat "Couldn't load the configuration for org-mode. Try again or remove the file init-org.el from the config folder"))))))

  (defvar dotemacs-org/journal-file (concat org-directory "/journal.org")
    "The path to the file where you want to make journal entries.")

  (defvar dotemacs-org/inbox-file (concat org-directory "/inbox.org")
    "The path to the file where to capture notes.")

  (unless (file-exists-p org-directory)
    (make-directory org-directory))

  (setq org-default-notes-file (expand-file-name dotemacs-org/inbox-file))
  (setq org-log-done t)
  (setq org-log-into-drawer t)

  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 2)
  (setq org-src-fontify-natively t)

(setq org-agenda-files `(,org-directory))
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline (expand-file-name dotemacs-org/inbox-file) "INBOX")
         "* TODO %?\n%U\n%a\n")
        ("n" "Note" entry
         (file+headline (expand-file-name dotemacs-org/inbox-file) "NOTES")
         "* %? :NOTE:\n%U\n%a\n")
        ("m" "Meeting" entry
         (file (expand-file-name dotemacs-org/inbox-file))
         "* MEETING %? :MEETING:\n%U")
        ("j" "Journal" entry
         (file+datetree (expand-file-name dotemacs-org/journal-file))
         "* %U\n** %?")))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d@)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

(setq org-todo-state-tags-triggers
        ' (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           ("TODO" ("WAITING") ("CANCELLED"))
           ("NEXT" ("WAITING") ("CANCELLED"))
           ("DONE" ("WAITING") ("CANCELLED"))))

  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-completion-use-ido t)

  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  (require-package 'ob-async)

  (after [evil org]
    (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)))

(provide 'init-org)
