;;; bindings-dired.el --- Dired bindings definitions

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;;; Code:
(after 'dired
  (bindings-define-keys dired-mode-map
    (";" #'dired-kill-subdir))

  (after "dired-subtree-autoloads"
    (bindings-define-keys dired-mode-map
      ((kbd "TAB") #'dired-subtree-toggle))))

(provide 'config-bindings-dired)
;;; bindings-dired.el ends here
