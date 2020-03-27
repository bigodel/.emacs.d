;;; bindings-company.el --- Bindings for company

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; note that i use <tab> and TAB. that is because <tab> is for window Emacs and
;; TAB is for terminal Emacs. it might be the case that TAB ecompasses both, but
;; I don't want to test it :P.
;;
;;; Code:
(after 'company
  ;; bindings that are used when company is activated
  (bindings-define-keys company-active-map
    ((kbd "C-n") #'company-complete-common-or-cycle)
    ((kbd "C-p") #'company-select-previous)
    ((kbd "<tab>") #'company-complete-selection)
    ((kbd "TAB") #'company-complete-selection))

  ;; make `company' work like vim's autocompletion
  (after 'evil
    (bindings-define-keys evil-insert-state-map
      ((kbd "<tab>") #'company-indent-or-complete-common)
      ((kbd "TAB") #'company-indent-or-complete-common)
      ((kbd "<C-tab>") #'company-yasnippet) ; this doesn't work on terminal
      ((kbd "C-TAB") #'company-yasnippet)   ; this doesn't work on terminal
      ((kbd "M-n") #'company-complete)      ; use M-{n,p} to force company
      ((kbd "M-p") #'company-complete))))

(provide 'bindings-company)
;;; bindings-company.el ends here
