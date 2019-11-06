;;; config-utils.el --- Some useful functions

;; Author: João Pedro de A. Paual <maybe_email_here@later>

;;; Commentary:

;;; Code:
(defun utils-reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load-file user-init-file))

(defun utils-window-killer ()
  "Close the window, and delete the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))

(defun utils-minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun utils-set-transparency (alpha)
  "Set the transparency of the current frame to ALPHA."
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha alpha))

(defun utils-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun utils-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    ;; if in evil normal or visual state get to Emacs state and move forward
    (after 'evil
      (when (evil-normal-state-p)
        (evil-emacs-state)
        (forward-char)
        (backward-kill-sexp)
        (insert (format "%s" value))
        (evil-normal-state)))
    (backward-kill-sexp)
    (insert (format "%s" value))))

(defun utils-rename-buffer-file (buffer)
  "Rename file associated to BUFFER."
  (interactive "bBuffer: ")
  (let ((filename (if (bufferp buffer)
                      (buffer-file-name buffer)
                    (buffer-file-name (get-file-buffer buffer)))))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name 1)
          (save-excursion
            (switch-to-buffer buffer)
            (rename-buffer new-name)
            (set-visited-file-name new-name t t)
            (set-buffer-modified-p nil))))))))

(defun utils-rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (utils-rename-buffer-file (current-buffer)))

;; TODO: delete-buffer-file
(defun utils-delete-current-buffer-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p
               (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun utils-goto-scratch-buffer ()
  "Create a new scratch buffer. If *scratch* already exists, switch to it."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun utils-insert-last-kbd-macro ()
  "Insert the last defined keyboard macro."
  (interactive)
  (name-last-kbd-macro 'my-last-macro)
  (insert-kbd-macro 'my-last-macro))

(defun utils-set-buffer-to-unix-format ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun utils-set-buffer-to-dos-format ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun utils-find-file-as-root (file)
  "Edit FILE as root."
  (interactive "f")
  (find-file-other-window (concat "/sudo::" file)))

(provide 'config-utils)
;;; config-utils.el ends here
