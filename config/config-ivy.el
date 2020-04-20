;;; config-ivy.el --- Ivy configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; `counsel' requires as dependency `ivy' and `swiper'
(require-package 'counsel)

;; start `counsel' and `ivy'
(counsel-mode t)
(ivy-mode t)

;; variables
(setvar 'ivy-use-virtual-buffers t) ; recentf & bookmarks in `ivy-switch-buffer'
(setvar 'ivy-count-format "%d/%d ")
(setvar 'ivy-wrap t)
(setvar 'ivy-height 16)
(setvar 'ivy-use-selectable-prompt t)  ; make the prompt selectable with C-p
(setvar 'ivy-initial-inputs-alist nil) ; don't have initial prompts ever

;; `counsel' automatically uses `amx' as the M-x package
(setvar 'amx-history-length 10)
(setvar 'amx-save-file (expand-file-name "amx-items" dotemacs-cache-directory))
(require-package 'amx)
(amx-mode)

;; add actions to `counsel-find-file'
(ivy-set-actions
 'counsel-find-file
 '(("d" delete-file "delete")))

;; `counsel' wrapper for tramp
(require-package 'counsel-tramp)
(setvar 'counsel-tramp-custom-connections '(/doas:root@localhost:/))

;; make swiper faster
(after 'swiper
  (defadvice swiper (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum))
  (defadvice swiper-all (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum)))

;; a nice little hydra to work with ivy
(after 'hydra
  (require-package 'ivy-hydra))

;; ivy on everything even projectile
(after 'projectile
  (require-package 'counsel-projectile)
  (counsel-projectile-mode t)

  ;; this an ivy interface to show todos on a project in projectile
  ;; NOTE: i got this from https://github.com/jsmestad/doom-todo-ivy
  (defvar ivy-task-tags
    '(("TODO"  . warning)
      ("FIXME" . error))
    "An list of tags for `ivy-tasks' to search for.")

  (defun ivy--tasks-candidates (tasks)
    "Generate a list of task candidates from TASKS."
    (let* ((max-type-width
            (cl-loop for task in ivy-task-tags maximize
                     (length (car task))))
           (max-desc-width
            (cl-loop for task in tasks maximize (length (cl-cdadr task))))
           (max-width (max (- (frame-width) (1+ max-type-width) max-desc-width)
                           25)))
      (cl-loop
       with fmt = (format "%%-%ds %%-%ds%%s%%s:%%s" max-type-width max-width)
       for alist in tasks
       collect
       (let-alist alist
         (format fmt
                 (propertize .type 'face (cdr (assoc .type ivy-task-tags)))
                 (substring .desc 0 (min max-desc-width (length .desc)))
                 (propertize " | " 'face 'font-lock-comment-face)
                 (propertize (abbreviate-file-name .file)
                             'face 'font-lock-keyword-face)
                 (propertize .line 'face 'font-lock-constant-face))))))

  (defun ivy--tasks (target)
    "Search TARGET for a list of tasks."
    (let* (case-fold-search
           (task-tags (mapcar #'car ivy-task-tags))
           (cmd
            (format "%s -H -S --no-heading -- %s %s"
                    (or (when-let* ((bin (executable-find "rg")))
                          (concat bin " --line-number"))
                        (when-let* ((bin (executable-find "ag")))
                          (concat bin " --numbers"))
                        (error "Cannot find executables: ripgrep or the_silver_searcher"))
                    (shell-quote-argument
                     (concat "\\s("
                             (string-join task-tags "|")
                             ")([\\s:]|\\([^)]+\\):?)"))
                    target)))
      (save-match-data
        (cl-loop with out = (shell-command-to-string cmd)
                 for x in (and out (split-string out "\n" t))
                 when (condition-case-unless-debug ex
                          (string-match
                           (concat "^\\([^:]+\\):\\([0-9]+\\):.+\\("
                                   (string-join task-tags "\\|")
                                   "\\):?\\s-*\\(.+\\)")
                           x)
                        (error
                         (message "Error matching task in file: (%s) %s"
                                  (error-message-string ex)
                                  (car (split-string x ":")))
                         nil))
                 collect `((type . ,(match-string 3 x))
                           (desc . ,(match-string 4 x))
                           (file . ,(match-string 1 x))
                           (line . ,(match-string 2 x)))))))


  (defun ivy--tasks-open-action (x)
    "Jump to the file X and line of the current task."
    (let ((location (cadr (split-string x " | ")))
          (type (car (split-string x " "))))
      (cl-destructuring-bind (file line) (split-string location ":")
        (with-ivy-window
          (find-file (expand-file-name file (projectile-project-root)))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line)))
          (search-forward type (line-end-position) t)
          (backward-char (length type))
          (recenter)))))

  (defun ivy-tasks (&optional arg)
    "Search through all `ivy-task-tags' in the current project or on ARG.
Optional ARG will search only that file."
    (interactive "P")
    (ivy-read (format "Tasks (%s): "
                      (if arg
                          (concat "in: " (file-relative-name buffer-file-name))
                        "project"))
              (ivy--tasks-candidates
               (ivy--tasks (if arg buffer-file-name (projectile-project-root))))
              :action #'ivy--tasks-open-action
              :caller 'ivy-tasks)))

(provide 'config-ivy)
;;; config-ivy.el ends here
