(load-theme 'manoj-dark t)

(line-number-mode t)
(column-number-mode t)
(display-time-mode t)
(size-indication-mode t)

(require-package 'origami)
(global-origami-mode)

(require-package 'diminish)
(diminish 'visual-line-mode)
(after 'aggressive-indent (diminish 'aggressive-indent-mode))
(after 'auto-complete (diminish 'auto-complete-mode))
(after 'autorevert (diminish #'auto-revert-mode))
(after 'color-identifiers-mode (diminish 'color-identifiers-mode))
(after 'company (diminish 'company-mode))
(after 'counsel (diminish #'counsel-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'evil-commentary (diminish 'evil-commentary-mode))
(after 'flycheck (diminish 'flycheck-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'helm-mode (diminish 'helm-mode))
(after 'hideshow (diminish 'hs-minor-mode))
(after 'highlight-symbol (diminish 'highlight-symbol-mode))
(after 'indent-guide (diminish 'indent-guide-mode))
(after 'ivy (diminish 'ivy-mode))
(after 'page-break-lines (diminish 'page-break-lines-mode))
(after 'projectile (diminish 'projectile-mode))
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'which-key (diminish 'which-key-mode))
(after 'yasnippet (diminish 'yas-minor-mode))

;; (require-package 'smart-mode-line)
;; (setq sml/theme 'dark)
;; (setq sml/no-confirm-load-theme t)
;; (sml/setup)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . 955) prettify-symbols-alist)
              (push '("return" . 8592) prettify-symbols-alist))))

(/boot/delayed-init
 (require-package 'color-identifiers-mode)
 (global-color-identifiers-mode)
 (diminish 'color-identifiers-mode))

(require-package 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(require-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require-package 'highlight-quoted)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)

(require-package 'page-break-lines)
(global-page-break-lines-mode)

(require-package 'eval-sexp-fu)
(require 'eval-sexp-fu)
(eval-sexp-fu-flash-mode)

(add-hook 'find-file-hook #'hl-line-mode)

(if (fboundp #'display-line-numbers-mode)
    (add-hook 'find-file-hook #'display-line-numbers-mode)
  (add-hook 'find-file-hook 'linum-mode))

(provide 'config-eyecandy)
