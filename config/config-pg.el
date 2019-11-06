;;; config-pg.el --- Proof General configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(require-package 'proof-general)

;;; proof-general variables
(setvar proof-strict-read-only 'retract)
(setvar proof-electric-terminator-enable t)
(setvar proof-three-window-mode-policy 'smart)
(setvar proof-splash-enable nil)
(setvar proof-script-fly-past-comments t)

;;; coq variables
(setvar coq-compile-before-require t)
;; (setvar coq-one-command-per-line t)

;; install `company-coq'
(after [company coq-mode]
  (require-package 'company-coq)
  (setvar company-coq-disabled-features '(smart-subscripts prettify-symbols))

  (defun /proof/coq-mode-hook ()
    "Add `company-coq' to `company-backends' and start `company-coq-mode'."
    (add-hook 'company-coq-mode-hook
              (lambda ()
                (setvar company-idle-delay 0.5 'local)))
    (company-coq-mode t))

  (add-hook 'coq-mode-hook #'/proof/coq-mode-hook))

(provide 'config-pg)
;;; config-pg.el ends here
