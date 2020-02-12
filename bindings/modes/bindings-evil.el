;;; bindings-evil.el --- Evil bindings definitions

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling. You can find all of Bailey Lings' Emacs
;; configuration in https://github.com/bling/dotemacs.
;;
;;; Code:
(after 'evil
  ;;; normal state maps
  (bindings-define-keys evil-normal-state-map
    ("gp" "`[v`]")
    ("[ " (bind (evil-insert-newline-above) (forward-line)) "new line up")
    ("] " (bind (evil-insert-newline-below) (forward-line -1)) "new line down"))

  ;; normal and visual state bindings
  (evil-define-key '(normal visual motion) 'global
    " " bindings-space-map
    " w" evil-window-map
    "gr" #'revert-buffer
    "[b" #'previous-buffer
    "]b" #'next-buffer
    "[q" #'previous-error
    "]q" #'next-error
    "Y" "y$"
    "gC" #'compile
    "gc" #'recompile
    "gA" #'align-regexp)

  ;; TODO: maybe don't add this
  ;; (after 'flycheck
  ;;   (evil-define-key 'normal flycheck-error-list-mode-map
  ;;     "j" #'flycheck-error-list-next-error
  ;;     "k" #'flycheck-error-list-previous-error))

  ;; (after 'diff-mode
  ;;   (evil-define-key 'normal diff-mode diff-mode-map
  ;;     "j" #'diff-hunk-next
  ;;     "k" #'diff-hunk-prev))

  (defun evil-mouse-yank-primary ()
    "Insert the contents of primary selection into the location
of point."
    (interactive)
    (let ((default-mouse-yank-at-point (symbol-value mouse-yank-at-point)))
      (setvar mouse-yank-at-point t)
      (mouse-yank-primary nil)
      (setvar mouse-yank-at-point default-mouse-yank-at-point)))

  (evil-define-key '(insert normal visual) 'global
    (kbd "<S-insert>") #'evil-mouse-yank-primary))

(provide 'bindings-evil)
;;; bindings-evil.el ends here
