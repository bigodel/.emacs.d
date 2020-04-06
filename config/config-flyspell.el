;;; config-flyspell.el --- Flyspell configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; install flyspell
(when (or (executable-find "aspell")
          (executable-find "ispell")
          (executable-find "hunspell"))
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(provide 'config-flyspell)
;;; config-flyspell.el ends here
