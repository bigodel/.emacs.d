;;; bindings-company.el --- Bindings for company

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(after 'company
  (bindings-define-keys company-active-map
    ((kbd "C-n") #'company-complete-common-or-cycle)
    ((kbd "C-p") #'company-select-previous)
    ((kbd "TAB") #'company-complete-common-or-cycle)
    ((kbd "<backtab>") #'company-select-previous)
    ((kbd "RET") #'company-complete-selection))

  ;; make `company' work like vim's autocompletion
  (after 'evil
    (bindings-define-keys evil-insert-state-map
      ((kbd "<C-tab>") #'company-yasnippet)
      ((kbd "C-n") #'company-complete)
      ((kbd "C-p") #'company-complete))))

(provide 'bindings-company)
;;; bindings-company.el ends here
