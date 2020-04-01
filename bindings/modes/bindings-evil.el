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
    ("gp" "`[v`]")                      ; TODO: what does this do?
    ("[ " (bind (evil-insert-newline-above) (forward-line)) "new line up")
    ("] " (bind (evil-insert-newline-below) (forward-line -1)) "new line down"))

  ;; TODO: add bindings to hideshow mode
  ;;; normal, visual and motion state bindings
  (evil-define-key '(normal visual motion) 'global
    " " bindings-space-map
    " w" evil-window-map
    (kbd "M-h") #'evil-window-left
    (kbd "M-j") #'evil-window-down
    (kbd "M-k") #'evil-window-up
    (kbd "M-l") #'evil-window-right
    "[b" #'previous-buffer
    "]b" #'next-buffer
    "Y" "y$"
    "K" #'man
    "gI" #'imenu
    (kbd "M-.") #'xref-find-definitions ; and this
    (kbd "M-?") #'xref-find-references  ; and this
    "gd" #'xref-find-definitions
    (kbd "C-]") #'xref-find-definitions
    "gr" #'xref-find-references)

  ;;; normal and visual state bindings
  (evil-define-key '(normal visual) 'global
    "[e" #'previous-error
    "]e" #'next-error
    "gC" #'compile
    "gc" #'recompile
    "gA" #'align-regexp)

  ;;; motion state bindings
  (evil-define-key 'motion 'global
    (kbd "<tab>") #'forward-button            ; evil overrides this
    (kbd "TAB") #'forward-button)             ; and this

  (defun evil-mouse-yank-primary ()
    "Insert the contents of primary selection into the location
of point."
    (interactive)
    (let ((default-mouse-yank-at-point (symbol-value mouse-yank-at-point)))
      (setvar 'mouse-yank-at-point t)
      (mouse-yank-primary nil)
      (setvar 'mouse-yank-at-point default-mouse-yank-at-point)))

  (evil-define-key '(insert normal visual) 'global
    (kbd "<S-insert>") #'evil-mouse-yank-primary))

(provide 'bindings-evil)
;;; bindings-evil.el ends here
