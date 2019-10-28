;;; config-pg.el --- Proof General configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(require-package 'proof-general)

;; proof-general settings
(setvar proof-strict-read-only 'retract)
(setvar proof-electric-terminator-enable t)
(setvar proof-three-window-mode-policy 'smart)
(setvar proof-splash-enable nil)
(setvar proof-script-fly-past-comments t)

;; coq settings
(setvar coq-compile-before-require t)
;; (setvar coq-one-command-per-line t)

(after [evil proof-script]
  (evil-ex-define-cmd "pr[ove]" 'proof-goto-point)
  (/bindings/define-keys proof-mode-map
    ((kbd "M-n") #'proof-assert-next-command-interactive "next command")
    ((kbd "M-p") #'proof-undo-last-successful-command "undo command")))

;; hooks
(add-hook
 'proof-mode-hook
 (lambda ()
   "Enable `undo-tree-mode', `aggressive-indent-mode',
`hl-todo-mode' and `flyspell-prog-mode' and disable
`holes-mode' and `prettify-symbols-mode'."
   (hl-todo-mode t)
   (aggressive-indent-mode t)
   (flyspell-prog-mode)
   (undo-tree-mode t)
   ;; (holes-mode -1)
   (prettify-symbols-mode -1)))

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
