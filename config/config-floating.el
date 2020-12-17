;;; config-floating.el --- Config of floating frames -*- lexical-binding: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; `hide-mode-line-mode'
;; this was mostly taken from https://github.com/hlissner/emacs-hide-mode-line,
;; with some small modifications to work specifically for my use case
(defvar hide-mode-line--old-format nil
  "Variable to store the old `mode-line-format'.")

(add-hook 'after-init-hook
          (lambda ()
            "Set the value of `hide-mode-line--default-format'
after initialization of Emacs."
            (defconst hide-mode-line--default-format
              mode-line-format
              "Constant to store the default value of `mode-line-format'.

Sometimes the `hide-mode-line--old-format' doesn't have the
correct value and is nil.")))

(defun hide-mode-line-reset ()
  "Reset `hide-mode-line-mode' in the current buffer, if necessary.

Sometimes, a major mode is activated after `hide-mode-line-mode'
is activated, thus disabling it (because changing major modes
invokes `kill-all-local-variables' and specifically kills
`mode-line-format''s local value, whether or not it's
permanent-local.

Attach this to `after-change-major-mode-hook' and
`hide-mode-line-mode' will be cycled to fix this."
  (when (bound-and-true-p hide-mode-line-mode)
    (hide-mode-line-mode -1)
    (hide-mode-line-mode t)))

(define-minor-mode hide-mode-line-mode
  "Toggles the `mode-line' on and off."
  :init-value nil
  :global nil
  (if hide-mode-line-mode
      (progn
        (add-hook 'after-change-major-mode-hook #'hide-mode-line-reset nil t)
        (setq hide-mode-line--old-format mode-line-format
              mode-line-format nil))
    (remove-hook 'after-change-major-mode-hook #'hide-mode-line-reset t)
    (if hide-mode-line--old-format
        (setq mode-line-format hide-mode-line--old-format
              hide-mode-line--old-format nil)
      (setq mode-line-format hide-mode-line--default-format
            hide-mode-line--old-format nil)))
  (force-mode-line-update))

;;; `delete-frame-mode'
(defun floating-delete-frame ()
  "Disable `delete-frame-mode' and call `delete-frame'."
  (interactive)
  (when (bound-and-true-p delete-frame-mode)
    (delete-frame-mode -1)
    (delete-frame)))

(define-minor-mode delete-frame-mode
  "Delete frame instead of buffer with `q'."
  :lighter " DEL"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") #'floating-delete-frame)
            map))

;;; `floating-frame-mode'
(define-minor-mode floating-frame-mode
  "Minor mode that enables `hide-mode-line-mode' and
`delete-frame-mode'."
  :lighter " Floating"
  :global nil
  (if floating-frame-mode
      (progn
        (hide-mode-line-mode t)
        (delete-frame-mode t))
    (hide-mode-line-mode -1)
    (delete-frame-mode -1)))

;;; floating functions
(defconst floating-initial-frame-alist
  `((left . 0.5)
    (top . 0.5)
    (width . 80)
    (height . 24)
    (font . "monospace-13"))
  "Frame alist for floating frames.

See `default-frame-alist' and `initial-frame-alist' for more
documentation and information.")

(defconst floating-calc-frame-alist
  (cons '(name . "calc") floating-initial-frame-alist)
  "Frame alist for the floating `calc'.

This is just a cons on `floating-initial-frame-alist' to
add `(name . \"calc\")'.")

(defun floating-calc ()
  "Open `calc' with `floating-frame-mode' enabled in both buffers."
  (interactive)
  (let ((after-change-major-mode-hook '(floating-frame-mode)))
    (select-frame (make-frame floating-calc-frame-alist))
    (full-calc)
    (floating-frame-mode)
    (unless (get-buffer "*Floating Calc*")
      (rename-buffer "*Floating Calc*"))))

(provide 'config-floating)
;;; config-floating.el ends here
