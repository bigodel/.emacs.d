;;; config-python.el --- Dart configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; `python-mode' configuration
;; (after 'python-mode)

;;; install `ein' for jupyter notebooks
(lazy-major-mode "\\.ipynb\\'" 'ein #'ein:run)

;;; `ein' configuration
;; (after 'ein)

(provide 'config-python)
;;; config-python.el ends here
