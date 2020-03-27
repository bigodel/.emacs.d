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
  "If the file has more tabs than (four) spaces, use tabs instead
for indentation. If it has more spaces, use spaces instead of
tabs."
  (interactive)
  (let ((space-count (how-many "^    " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (when (> tab-count space-count)
      (setvar indent-tabs-mode t 'local))))

(add-hook 'find-file-hook #'infer-indentation-style)

;;; jump to definitions
;; TODO: maybe don't use this or if using move bindigns to appropriate file
;; (require-package 'dumb-jump)
;; (setvar dumb-jump-mode-selector 'ivy)
;; (if (executable-find "rg")
;;     (setvar dumb-jump-prefer-searcher 'rg)
;;   (when (executable-find "ag")
;;     (setvar dumb-jump-prefer-searcher 'ag)))

;; (after [evil dumb-jump]
;;   (/bindings/define-key evil-normal-state-map (kbd "g d") #'dumb-jump-go)
;;   (defadvice dumb-jump-go (before dotemacs activate)
;;     (evil-set-jump)))

;; (dumb-jump-mode))

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
