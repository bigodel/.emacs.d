;;; bindings-company.el --- Bindings for company

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; note that i use <tab> and TAB. that is because <tab> is for window Emacs and
;; TAB is for terminal Emacs. it might be the case that TAB ecompasses both, but
;; I don't want to test it :P. the dolist snippet of code i got from this post:
;; https://emacs.stackexchange.com/questions/13286/how-can-i-stop-the-enter-key-from-triggering-a-completion-in-company-mode
;;
;;; Code:
(after 'company
  ;; bindings that are used when company is activated
  (bindings-define-keys company-active-map
    ((kbd "C-n") #'company-complete-common-or-cycle)
    ((kbd "C-p") #'company-select-previous)
    ((kbd "<tab>") #'company-complete-selection)
    ((kbd "TAB") #'company-complete-selection))
  ;; <return> is for windowed Emacs; RET is for terminal Emacs
  (dolist (key '("<return>" "RET"))
    ;; here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))

  ;; make `company' work like vim's autocompletion
  (after [evil yasnippet]
    (bindings-define-keys evil-insert-state-map
      ;; ((kbd "<tab>") #'company-indent-or-complete-common)
      ;; ((kbd "TAB") #'company-indent-or-complete-common)
      ;; ((kbd "<C-tab>") #'company-yasnippet) ; this doesn't work on terminal
      ;; ((kbd "C-TAB") #'company-yasnippet)   ; this doesn't work on terminal
      ((kbd "M-n") #'company-complete)      ; use M-{n,p} to force company
      ((kbd "M-p") #'company-complete)
      ((kbd "M-N") #'company-yasnippet)
      ((kbd "M-P") #'company-yasnippet))))

(provide 'bindings-company)
;;; bindings-company.el ends here
