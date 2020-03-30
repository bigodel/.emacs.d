;;; config-prog.el --- Prog mode configurations

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; General configurations for `prog-mode' and some modes that don't come pre
;; installed with Emacs, like `vimrc-mode'.
;;
;;; Code:
;;; infer indentation
(defun infer-indentation-style ()
  "Automatically infer if indentation is using spaces or tabs.

If the file has more tabs than (four) spaces, use tabs instead
for indentation. If it has more spaces, use spaces instead of
tabs."
  (interactive)
  (let ((space-count (how-many "^    " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (when (> tab-count space-count)
      (setvar 'indent-tabs-mode t 'local))))

(add-hook 'prog-mode-hook #'infer-indentation-style)

;;; code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;;; bunch of modes that don't come with emacs
(lazy-major-mode "\\.toml\\'" toml-mode)
(lazy-major-mode "\\.yaml\\'" yaml-mode)
(lazy-major-mode "\\.json\\'" json-mode)
(lazy-major-mode "\\.exrc\\'" vimrc-mode)
(lazy-major-mode "[._]?g?vimrc\\'" vimrc-mode)
(lazy-major-mode "\\.vim\\'" vimrc-mode)
(lazy-major-mode "\\.lua\\'" lua-mode)
(lazy-major-mode "\\.csv\\'" csv-mode)
(lazy-major-mode "\\.?cron\\() (tab\\)?\\'" crontab-mode)

;;; aggressive indent
(defconst dotemacs-misc-aggressive-indent-hooks
  '(c-mode-hook
    cc-mode-hook
    lisp-mode-hook
    emacs-lisp-mode-hook
    sh-mode-hook
    java-mode-hook
    python-mode-hook
    makefile-mode-hook
    dart-mode-hook
    proof-mode-hook)
  "Hooks for major modes to activate `aggressive-indent-mode'.")

(require-package 'aggressive-indent)
(dolist (hook dotemacs-misc-aggressive-indent-hooks)
  (add-hook hook #'aggressive-indent-mode))

(provide 'config-prog.el)
;;; config-prog.el ends here
