;;; bindings-evil.el --- Evil bindings definitions

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling. You can find all of Bailey Lings' Emacs
;; configuration in https://github.com/bling/dotemacs.
;;
;;; Code:
(after 'evil
  ;; normal and visual state bindings
  (evil-define-key '(normal visual) 'global
    "Y" "y$"
    "gC" #'compile
    "gc" #'recompile
    "gA" #'align-regexp)

  (defun evil-mouse-yank-primary ()
    "Insert the contents of primary selection into the location
of point."
    (interactive)
    (let ((default-mouse-yank-at-point (symbol-value mouse-yank-at-point)))
      (setvar mouse-yank-at-point t)
      (mouse-yank-primary nil)
      (setvar mouse-yank-at-point default-mouse-yank-at-point)))

  ;; TODO: move to bindings-evil.el
  (evil-define-key '(insert normal visual) 'global
    (kbd "<S-insert>") #'evil-mouse-yank-primary))

(provide 'bindings-evil)
;;; bindings-evil.el ends here
