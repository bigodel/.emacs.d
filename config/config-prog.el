;;; config-prog.el --- Prog mode configurations

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; General configurations for `prog-mode' and some modes that don't come pre
;; installed with Emacs, like `vimrc-mode'.
;;
;;; Code:
(after 'prog-mode
  ;;; highlight TODO in prog-mode
  (require-package 'hl-todo)
  (add-hook 'prog-mode-hook #'hl-todo-mode)

  ;;; jump to definitions
  (require-package 'dumb-jump)
  (setvar dumb-jump-mode-selector 'ivy)
  (if (executable-find "rg")
      (setvar dumb-jump-prefer-searcher 'rg)
    (when (executable-find "ag")
      (setvar dumb-jump-prefer-searcher 'ag)))

  (after [evil dumb-jump]
    (/bindings/define-key evil-normal-state-map (kbd "g d") #'dumb-jump-go)
    (defadvice dumb-jump-go (before dotemacs activate)
      (evil-set-jump)))

  (dumb-jump-mode))

;;; bunch of modes that don't come with emacs
(/boot/lazy-major-mode "\\.toml$" toml-mode)
(/boot/lazy-major-mode "\\.yaml$" yaml-mode)
(/boot/lazy-major-mode "\\.json$" json-mode)
(/boot/lazy-major-mode "\\.exrc\\'" vimrc-mode)
(/boot/lazy-major-mode "[._]?g?vimrc\\'" vimrc-mode)
(/boot/lazy-major-mode "\\.vim\\'" vimrc-mode)
(/boot/lazy-major-mode "\\.lua$" lua-mode)
(/boot/lazy-major-mode "\\.csv$" csv-mode)
(/boot/lazy-major-mode "\\.?cron\\() (tab\\)?\\'" crontab-mode)
