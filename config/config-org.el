;;; config-org.el --- Org mode configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(after 'org
  (setvar org-directory (concat (getenv "HOME") "/docs/org"))

  ;; ask to create `org-directory' if non-existent and if it fails ask to use
  ;; ~/Documents/Org as the `org-directory'
  (unless (file-directory-p org-directory)
    (create-non-existent-directory org-directory)
    (unless (file-directory-p org-directory)
      (let ((default-org (concat (getenv "HOME") "/Documents/Org")))
        (if (y-or-n-p
             (format
              "Failed to create `%s', use the default directory [%s]?"
              org-directory default-org))
            (progn (make-directory (concat (getenv "HOME") "/Documents/Org") t)
                   (setvar org-directory (concat (getenv "HOME") "/Documents")))
          (error (concat "Couldn't load the configuration for `org-mode'.
Try again or remove the file `%s' from the config folder" load-file-name))))))

  (defconst dotemacs-org/journal-file (concat org-directory "/journal.org")
    "The path to the file where you want to make journal entries.")

  (defconst dotemacs-org/inbox-file (concat org-directory "/inbox.org")
    "The path to the file where to capture notes.")

  (setvar org-startup-indented t)
  (setvar org-src-fontify-natively t)

  (setvar org-agenda-files (list dotemacs-org/inbox-file))

  ;; (setvar org-default-notes-file (expand-file-name dotemacs-org/inbox-file))
  ;; (setvar org-log-done t)
  ;; (setvar org-log-into-drawer t)

  ;; (setvar org-agenda-files `(,org-directory))
  ;; (setvar org-capture-templates
  ;;         '(("t" "Todo" entry
  ;;            (file+headline (expand-file-name dotemacs-org/inbox-file) "INBOX")
  ;;            "* TODO %?\n%U\n%a\n")
  ;;           ("n" "Note" entry
  ;;            (file+headline (expand-file-name dotemacs-org/inbox-file) "NOTES")
  ;;            "* %? :NOTE:\n%U\n%a\n")
  ;;           ("m" "Meeting" entry
  ;;            (file (expand-file-name dotemacs-org/inbox-file))
  ;;            "* MEETING %? :MEETING:\n%U")
  ;;           ("j" "Journal" entry
  ;;            (file+datetree (expand-file-name dotemacs-org/journal-file))
  ;;            "* %U\n** %?")))

  ;; (setvar org-use-fast-todo-selection nil)
  ;; (setvar org-treat-S-cursor-todo-selection-as-state-change nil)
  ;; (setvar org-todo-keywords
  ;;         '((sequence "TODO(t)" "NEXT(n@)" "|" "DONE(d@)")
  ;;           (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

  ;; (setvar org-todo-state-tags-triggers
  ;;         ' (("CANCELLED" ("CANCELLED" . t))
  ;;            ("WAITING" ("WAITING" . t))
  ;;            ("TODO" ("WAITING") ("CANCELLED"))
  ;;            ("NEXT" ("WAITING") ("CANCELLED"))
  ;;            ("DONE" ("WAITING") ("CANCELLED"))))

  ;; (setvar org-refile-targets '((nil :maxlevel . 9)
  ;;                              (org-agenda-files :maxlevel . 9)))
  ;; (setvar org-refile-use-outline-path 'file)
  ;; (setvar org-outline-path-complete-in-steps nil)
  ;; (setvar org-completion-use-ido t)

  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

  ;; (after 'org
  ;;   (/bindings/define-keys (current-global-map)
  ;;     ("C-c ")))
  )

(provide 'config-org)
;;; config-org.el ends here
