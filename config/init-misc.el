(after 'dired
  (require-package 'dired-k)
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t)
  (add-hook 'dired-initial-position-hook #'dired-k))

(require-package 'undo-tree)
(setq undo-tree-auto-save-history t)
(setq undo-tree-enable-undo-in-region nil)
(setq undo-tree-history-directory-alist
      `(("." . ,(concat dotemacs-cache-directory "undo/"))))
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(global-undo-tree-mode)

(require-package 'dumb-jump)
(after [evil dumb-jump]
       (defadvice dumb-jump-go (before dotemacs activate)
         (evil-set-jump)))

(when (executable-find "ag")
  (require-package 'ag)
  (require 'ag)
  (setq ag-highlight-search t)
  (setq ag-ignore-list dotemacs-globally-ignored-directories)
  (add-hook 'ag-mode-hook (lambda () (toggle-truncate-lines t))))

(require-package 'expand-region)

(require-package 'aggressive-indent)
(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)

(require-package 'popwin)
(require 'popwin)
(push '(compilation-mode :noselect t) popwin:special-display-config)
(popwin-mode)

(provide 'init-misc)
