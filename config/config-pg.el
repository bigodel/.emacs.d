;;; config-pg.el --- Proof General configuration -*- lexical-bindings: t; -*-

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
(setvar 'coq-one-command-per-line nil)
(setvar 'coq-holes-minor-mode nil)
(setvar 'coq-mode-abbrev-table '())

(after 'coq-mode
  ;; coq abbrevs
  (define-abbrev-table 'coq-mode-abbrev-table '())
  (define-abbrev coq-mode-abbrev-table "ref" "reflexivity.")
  (define-abbrev coq-mode-abbrev-table "ind" "induction")
  (define-abbrev coq-mode-abbrev-table "sim" "simpl."))

;; install `company-coq'
;; (after [company coq-mode]
;;   (require-package 'company-coq)
;;   ;; deactivate some features
;;   (setvar 'company-coq-disabled-features
;;           '(smart-subscripts            ; self explanatory
;;             prettify-symbols            ; self explanatory
;;             company-defaults))          ; automatic company config

;;   ;; activate company-coq
;;   (add-hook 'coq-mode-hook #'company-coq-mode))

;;; hooks and misc config
(after [proof-site undo-tree]
  ;; this series of functions make it so that undo-tree will sort of behave with
  ;; proof-general. by default, PG disables undo-tree. there is an open issue on
  ;; github where discussion is been had and where i took these functions from:
  ;; https://github.com/ProofGeneral/PG/issues/430
  (defun pg-in-protected-region-p ()
    "Check if point is in locked region or not."
    (< (point) (proof-queue-or-locked-end)))

  (defmacro pg-wrap-edit (action)
    "Macro to wrap ACTION and only act on not locked region."
    `(if (or (not proof-locked-span)
             (equal (proof-queue-or-locked-end) (point-min)))
         (,action)
       (,action)
       (when (pg-in-protected-region-p)
         (proof-goto-point))))

  (defun pg-undo ()
    "Protected `undo-tree-undo' for Proof General."
    (interactive)
    (pg-wrap-edit undo-tree-undo))

  (defun pg-redo ()
    "Protected `undo-tree-redo' for Proof General."
    (interactive)
    (pg-wrap-edit undo-tree-redo))

  ;; proof-general disables `undo-tree-mode'
  (add-hook 'proof-general-mode-hook #'undo-tree-mode)

  (after 'coq-mode
    (add-hook 'coq-mode-hook #'undo-tree-mode)))

(provide 'config-pg)
;;; config-pg.el ends here
