
(defun /util/window-killer ()
  "Closes the window, and deletes the buffer if it's the last window open."
  (interactive)
  (if (> buffer-display-count 1)
      (if (= (length (window-list)) 1)
          (kill-buffer)
        (delete-window))
    (kill-buffer-and-window)))

(defun /util/minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun /util/set-transparency (alpha)
  "Sets the transparency of the current frame."
  (interactive "nAlpha: ")
  (set-frame-parameter nil 'alpha alpha))

(defun /util/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun /util/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%s" value))))

(defun /util/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun /util/delete-current-buffer-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun /util/goto-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun /util/insert-last-kbd-macro ()
  (interactive)
  (name-last-kbd-macro 'my-last-macro)
  (insert-kbd-macro 'my-last-macro))

(defun /util/set-buffer-to-unix-format ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun /util/set-buffer-to-dos-format ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun /util/find-file-as-root (file)
  "Edits a file as root."
  (interactive "f")
  (find-file-other-window (concat "/sudo:root@localhost:" file)))

(defun /util/tangle-init ()
  (interactive)
  "If the current buffer is init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(provide 'init-util)
