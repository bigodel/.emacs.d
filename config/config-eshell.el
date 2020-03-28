;;; config-eshell.el --- Eshell configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; variables
(defconst dotemacs-eshell-visual-commands
  '("ssh" "top" "tail" "less")
  "Command that present their output in a visual fashion.")

(setvar 'eshell-buffer-maximum-lines 20000)
(setvar 'eshell-scroll-to-bottom-on-input 'this)
(setvar 'eshell-buffer-shorthand t)
(setvar 'eshell-glob-case-insensitive nil)
(setvar 'eshell-error-if-no-glob t)
(setvar 'eshell-history-size (* 10 1024))
(setvar 'eshell-hist-ignoredups t)
(setvar 'eshell-cmpl-ignore-case t)

;; add some more commands to the visual commands
(after 'em-term
  (dolist (cmd dotemacs-eshell/visual-commands)
    (add-to-list 'eshell-visual-commands cmd)))

;;; helper packages
(require-package 'esh-help)             ; show minibuffer help like eldoc
(setup-esh-help-eldoc)                  ; setup esh-help

;;; advices
(defadvice eshell/exit (before dotemacs activate)
  "After exiting `eshell', remove its window."
  (delete-window))

(when (executable-find "fortune")
  (defadvice eshell (before dotemacs activate)
    (setvar 'eshell-banner-message
            (concat (shell-command-to-string "fortune") "\n"))
    "Display a little `fortune' at `eshell's startup."))

;;; functions and aliases
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

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

(after 'magit
  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo)))

(defun eshell/new-window ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         ;; (width (/ (window-total-height) 2))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    ;; (split-window-horizontally)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert "ls")
    (eshell-send-input)))

(defun eshell/ec (&rest args)
  "Compile ARGS in Emacs. Use `compile' to do background make."
  (if (eshell-interactive-output-p)
      (let ((compilation-process-setup-function
             (list 'lambda nil
                   (list 'setq 'process-environment
                         (list 'quote (eshell-copy-environment))))))
        (compile (eshell-flatten-and-stringify args))
        (pop-to-buffer compilation-last-buffer))
    (throw 'eshell-replace-command
           (let ((l (eshell-stringify-list (eshell-flatten-list args))))
             (eshell-parse-command (car l) (cdr l))))))
(put 'eshell/ec 'eshell-no-numeric-conversions t)

(defun eshell/view-file (file)
  "View FILE. A version of `view-file' which properly rets the
eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((buffer (find-file-noselect file)))
    (if (eq (get (buffer-local-value 'major-mode buffer) 'mode-class)
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))

(defun eshell/less (&rest args)
  "Invoke `view-file' on a file (ARGS). \"less +42 foo\" will go
to line 42 in the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (eshell/view-file file)
          (forward-line line))
      (eshell/view-file (pop args)))))

(provide 'config-eshell)
;;; config-eshell.el ends here
