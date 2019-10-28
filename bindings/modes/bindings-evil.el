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
    "gA" #'align-regexp))

(provide 'bindings-evil)
;;; bindings-evil.el ends here
