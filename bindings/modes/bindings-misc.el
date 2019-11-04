;;; bindings-misc.el --- Miscellaneous bindings (see config/config-misc.el)

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling. You can find all of Bailey Lings' Emacs
;; configuration in https://github.com/bling/dotemacs.
;;
;;; Code:
;;; pdf-tools
(after 'pdf-tools
  (bindings-define-keys pdf-view-mode-map
    ("q" #'utils-window-killer)
    ("k" nil)
    ("j" #'pdf-view-next-line-or-next-page)
    ("k" #'pdf-view-previous-line-or-previous-page)
    ("J" #'pdf-view-next-page)
    ("K" #'pdf-view-previous-page)))

;;; helpful
(bindings-define-keys (current-global-map)
  ((kbd "C-h f") #'helpful-callable "describe function")
  ((kbd "C-h v") #'helpful-variable "describe variable")
  ((kbd "C-h F") #'helpful-command "describe command")
  ((kbd "C-h k") #'helpful-key "describe key")
  ((kbd "C-c C-d") #'helpful-at-point "help for symbol at point"))
(after 'helpful
  (bindings-define-keys helpful-mode-map
    ("q" #'quit-window)
    ([tab] #'forward-button)))

(provide 'bindings-misc)
;;; bindings-misc.el ends here
