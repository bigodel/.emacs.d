;;; config-org.el --- Org mode configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; TODO: create a default file with some configuration that should be shared
;; among all org file such as author, email and languages (en, pt_BR)
(setvar 'org-directory (expand-file-name "docs/org" (getenv "HOME")))

;; ask to create `org-directory' if non-existent and if it fails ask to use
;; ~/Documents/Org as the `org-directory'
(unless (file-directory-p org-directory)
  (create-non-existent-directory org-directory)
  (unless (file-directory-p org-directory)
    (let ((default-org (expand-file-name "Documents/Org" (getenv "HOME"))))
      (if (y-or-n-p
           (format
            "Failed to create `%s', use the default directory [%s]?"
            org-directory default-org))
          (progn (make-directory (expand-file-name "Documents/Org"
                                                   (getenv "HOME")) t)
                 (setvar 'org-directory (expand-file-name "Documents"
                                                          (getenv "HOME"))))
        (error (concat "Couldn't load the configuration for `org-mode'.
Try again or remove the file `%s' from the config folder" load-file-name))))))

;;; variables
(setvar 'org-cycle-separator-lines 2) ; number os lines to keep between headers
(setvar 'org-startup-indented t)      ; startup indented?
(setvar 'org-startup-truncated t)     ; truncate lines in org? D:<
(setvar 'org-startup-with-inline-images t) ; show inline images on startup
;; the width of images in pixels. note that this will stretch images that aren't
;; the pixel width set here, if it is a numeric value.
(setvar 'org-image-actual-width (/ (display-pixel-width) 8))
(setvar 'org-src-fontify-natively t)  ; fontify src code blocks?
(setvar 'org-src-preserve-indentation t) ; preserve indentation (look the doc)
(setvar 'org-return-follows-link t)   ; return works like C-c C-o
(setvar 'org-preview-latex-image-directory (expand-file-name ; latex preview
                                            "ltximg/"        ; image location
                                            (if (getenv "TMPDIR")
                                                (getenv "TMPDIR")
                                              "/tmp")))

;;; constants
(defconst org-inbox-file (expand-file-name "inbox.org" org-directory)
  "The path to the file where to capture notes.")

(defconst org-notes-file (expand-file-name "notes.org" org-directory)
  "The path to the file where to add notes, cheatsheets and etc.")

(defconst org-secrets-file
  (expand-file-name "secrets.org" (concat (or (getenv "XDG_DOCUMENTS_DIR")
                                              "~/docs") "/secret"))
  "The path to the file with sensitive information that I don't want public.")

(defconst org-journal-file (expand-file-name "journal.org" org-directory)
"The path to the file where you want to make journal entries.")

;;; agenda configuration
(setvar 'org-agenda-files
        `(,org-inbox-file
          ,(expand-file-name "calendar.org" org-directory)
          ,(expand-file-name "work.org"     org-directory)
          ,(expand-file-name "uni.org"      org-directory)
          ,(expand-file-name "personal.org" org-directory))
        nil "Where to look for TODO's for the agenda.")
;; TODO if there is too much clutter in the agenda, set these to t
(setvar 'org-agenda-skip-timestamp-if-done nil) ; skip timestamped task if done
(setvar 'org-agenda-skip-deadline-if-done nil) ; skip deadline task if done
(setvar 'org-agenda-skip-scheduled-if-done nil) ; skip scheduled task if done
(setvar 'org-agenda-window-setup 'only-window)  ; how to show the agenda window
(setvar 'org-agenda-restore-windows-after-quit t) ; restore window configuration
(setvar 'org-agenda-show-all-dates t)             ; show every date?
;; NOTE if this is set to nil then it will always start on the current day!
(setvar 'org-agenda-start-on-weekday 0)       ; day to start the agenda on
(setvar 'org-agenda-include-diary t)          ; include Emacs diary entries
(setvar 'calendar-date-style 'european)       ; format of dates in the calendar

(defun org-diary-last-day-of-month (date)
  "Return t if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

;;; capture configuration
;; TODO: make better capture templates
(setvar 'org-capture-templates
        '(("t" "task" entry
           (file org-inbox-file)
           "* TODO %?\n:LOGBOOK:\n- Captured on %U\n:END:\n[[%F][%f]]\n\n"
           :empty-lines-before 1 :empty-lines-after 2)
          ("q" "question" entry
           (file org-inbox-file)
           "* QUESTION %? :question:\n:LOGBOOK:\n- Captured on %U\n:END:\n\n"
           :empty-lines-before 1 :empty-lines-after 2)
          ("n" "note" entry
           (file+headline org-notes-file "Refile") "* %? :refile:\n:LOGBOOK:\n- Captured on %U\n:END:\n\n"
           :empty-lines-before 1 :empty-lines-after 2)
          ("e" "event" entry
           (file org-inbox-file) "* %? :event:\n%^T\n:LOGBOOK:\n- Captured on %U\n:END:\n\n"
           :empty-lines-before 1 :empty-lines-after 2)
          ("a" "appointment" entry
           (file org-inbox-file) "* %? \n%^T\n:LOGBOOK:\n- Captured on %U\n:END:\n\n"
           :empty-lines-before 1 :empty-lines-after 2)))
;; default file for capturing
(setvar 'org-default-notes-file org-inbox-file)

;;; TODO's
;; (setvar 'org-treat-S-cursor-todo-selection-as-state-change nil)
(setvar 'org-enforce-todo-checkbox-dependencies t) ; don't allow to change to DONE
(setvar 'org-enforce-todo-dependencies t) ; until everything is really done
(setvar 'org-hierarchical-todo-statistics nil) ; stats should cover whole tree
(setvar 'org-log-into-drawer t)           ; log state changes in a drawer
(setvar 'org-log-done 'note)
(setvar 'org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n/!)" "STARTED(s@)" "|" "DONE(d@)")
          (sequence "QUESTION(q!)" "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

(setvar 'org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "orange" :weight bold)
          ("STARTED" :foreground "cyan" :weight bold)
          ("DONE" :foreground "green" :weight bold)
          ("QUESTION" :foreground "magenta" :weight bold)
          ("WAITING" :foreground "yellow" :weight bold)
          ("CANCELLED" :foreground "dark red" :weight bold)))

(setvar 'org-todo-state-tags-triggers
        '(("CANCELLED" ("cancelled" . t))
          ("WAITING" ("waiting" . t))
          ("QUESTION" ("question" . t))
          ("TODO" ("waiting") ("cancelled") ("question"))
          ("NEXT" ("waiting") ("cancelled") ("question"))
          ("STARTED" ("waiting") ("cancelled") ("question"))
          ("DONE" ("waiting") ("cancelled") ("question"))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" 'none))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;; refile configuration
(setvar 'org-refile-targets '((org-agenda-files :maxlevel . 9)
                              (org-notes-file :maxlevel . 9)))
(setvar 'org-refile-use-outline-path 'file) ; show the filename on refiling
;; we need to tell org to don't complete in steps since we're using ivy
(setvar 'org-outline-path-complete-in-steps nil)
;; allow us to create new headings when refiling but confirm the creation
(setvar 'org-refile-allow-creating-parent-nodes 'confirm)
;; (setvar 'org-completion-use-ido t)

;; save every time we make a modification on our agenda or refile
;; got this from
;; https://emacs.stackexchange.com/questions/21754/how-to-automatically-save-all-org-files-after-marking-a-repeating-item-as-done-i
;; https://emacs.stackexchange.com/questions/26923/org-mode-getting-errors-when-auto-saving-after-refiling
(advice-add 'org-deadline :after
            (lambda (&rest _) (funcall #'org-save-all-org-buffers)))
(advice-add 'org-schedule :after
            (lambda (&rest _) (funcall #'org-save-all-org-buffers)))
(advice-add 'org-store-log-note :after
            (lambda (&rest _) (funcall #'org-save-all-org-buffers)))
(advice-add 'org-todo :after
            (lambda (&rest _) (funcall #'org-save-all-org-buffers)))
(advice-add 'org-clock-in :after
            (lambda (&rest _) (funcall #'org-save-all-org-buffers)))
(advice-add 'org-clock-out :after
            (lambda (&rest _) (funcall #'org-save-all-org-buffers)))
(advice-add 'org-refile :after
            (lambda (&rest _)
              (funcall #'org-save-all-org-buffers)))

;;; additional modules and variables that are loaded with org
(after 'org
  ;; remove the default pdf rule for opening org export files and add my own
  (delete '("\\.pdf\\'" . default) org-file-apps)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

  (setvar 'org-format-latex-options     ; make latex preview bigger
          (plist-put org-format-latex-options :scale 1.8))

  (setvar 'org-habit-graph-column 100)  ; column at which to show the graph
  ;; add `org-habit' to the loaded modules
  (add-to-list 'org-modules 'org-habit t)

  ;; i tried using `org-checklist.el' but it is not working, so i just got what
  ;; i need to reset the checkboxes here
  (defun org-reset-checkbox-state-maybe ()
    "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES'
property is set"
    (interactive "*")
    (if (org-entry-get (point) "RESET_CHECK_BOXES")
        (org-reset-checkbox-state-subtree)))

  (defun org-checklist ()
    "Call `org-reset-checkbox-state-maybe' if in a not DONE
task."
    (when (member org-state org-done-keywords) ; org-state dynamically bound in
      (org-reset-checkbox-state-maybe))) ; org.el/org-todo

  (add-hook 'org-after-todo-state-change-hook 'org-checklist)

  ;;; exporting
  (setvar 'org-export-backends (cons 'md org-export-backends))

  ;; github flavored markdown
  (require-package 'ox-gfm)
  (setvar 'org-export-backends (cons 'gfm org-export-backends)))

;;; {La}TeX configuration
;; TODO: create an article.tex inside tex/ for article templates and other kinds
;; of latex templates; this might be an yasnippet snippet in the future as well
;; and it would be a snippet based on this template (ideally)

;;; misc
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

(after 'whitespace
  (add-hook 'org-mode-hook
            (lambda ()
              "Disable 'lines-tail and 'empty for
`whitespace-mode' in `org-mode'. The former is because links
actually do go over the 80 column limit, but on the `org-mode'
visualization of things they don't; and the latter is because for
some reason it does not behave very well, accusing lines that I'm
currently editing as being empty and other strange behaviors."
              (setvar 'whitespace-line-column nil 'local)
              (setvar 'whitespace-style
                      (remove 'lines-tail whitespace-style) 'local)
              (setvar 'whitespace-style
                      (remove 'empty whitespace-style) 'local)
              (whitespace-mode -1)
              (whitespace-mode t))))

;;; third party packages
;; beautiful bullets
(require-package 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)
;;; config-org.el ends here
