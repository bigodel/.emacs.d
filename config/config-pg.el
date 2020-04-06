;;; config-pg.el --- Proof General configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; TODO: only install if visiting one of the files that proof general handles!
;;; install proof-general
(require-package 'proof-general)

;;; proof-general variables
(setvar 'proof-strict-read-only 'retract)
(setvar 'proof-electric-terminator-enable t)
(setvar 'proof-three-window-mode-policy 'smart)
(setvar 'proof-splash-enable nil)
(setvar 'proof-script-fly-past-comments t)

;;; coq variables
(setvar 'coq-compile-before-require t)
;; (setvar 'coq-one-command-per-line t)

;; install `company-coq'
(after [company coq-mode]
  (require-package 'company-coq)
  ;; deactivate some features
  (setvar 'company-coq-disabled-features '(smart-subscripts ; self explanatory
                                           prettify-symbols ; self explanatory
                                           company-defaults)) ; some company conf

  ;; activate company-coq
  (add-hook 'coq-mode-hook #'company-coq-mode))

(provide 'config-pg)
;;; config-pg.el ends here
