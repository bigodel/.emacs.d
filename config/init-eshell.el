(defvar dotemacs-eshell/prompt-git-info
  (executable-find "git")
  "Full path for the `git' executable.")

(defvar dotemacs-eshell/visual-commands
  '("ssh" "top" "tail" "less")
  "Command that present their output in a visual fashion.")

(defadvice eshell/exit (before dotemacs activate)
  "After exiting `eshell', remove its window."
  (delete-window))

(setq eshell-directory-name (concat dotemacs-cache-directory "eshell"))
(setq eshell-buffer-maximum-lines 20000)
(setq eshell-scroll-to-bottom-on-input 'this)
(setq eshell-buffer-shorthand t)
(setq eshell-aliases-file (concat user-emacs-directory "alias"))
(setq eshell-glob-case-insensitive t)
(setq eshell-error-if-no-glob t)
(setq eshell-history-size (* 10 1024))
(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-ignore-case t)

(when (executable-find "fortune")
  (defadvice eshell (before dotemacs activate)
    (setq eshell-banner-message
          (concat (shell-command-to-string "fortune") "\n"))
    "Display a little `fortune' at `eshell's startup."))

(defun eshell/ff (&rest args)
  "Opens a file in emacs."
  (when (not (null args))
    (mapc #'find-file (mapcar #'expand-file-name
                              (eshell-flatten-list (reverse args))))))

(defun eshell/h ()
  "Quickly run a previous command."
  (insert (completing-read
           "Run previous command: "
           (delete-dups (ring-elements eshell-history-ring))
           nil
           t))
  (eshell-send-input))

(defun eshell/tramp (&rest args)
  "Use tramp as a eshell command."
  (insert (apply #'format "cd /ssh:%s:\\~/" args))
  (eshell-send-input))

(after 'em-term
  (dolist (cmd dotemacs-eshell/visual-commands)
    (add-to-list 'eshell-visual-commands cmd)))

(after "magit-autoloads"
  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo)))

(defun /eshell/new-window ()
  "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 2))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(provide 'init-eshell)
