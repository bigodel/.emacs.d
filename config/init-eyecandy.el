(require-package 'solarized-theme)
(setq solarized-scale-org-headlines nil)
(setq x-underline-at-descent-line t)
(require 'solarized-theme)

(require-package 'monokai-theme)

(require-package 'gruvbox-theme)

(load-theme 'manoj-dark t)

;; change fringe background and foreground color
(set-face-attribute 'fringe nil
                    :background (face-background 'default)
                    :foreground (face-foreground 'default))

;; make comments grey (manoj-dark)
(set-face-foreground 'font-lock-comment-face "dimgray")
(set-face-foreground 'font-lock-comment-delimiter-face "dimgray")

;; disable the bigger scale on bold function fonts (manoj-dark)
(set-face-attribute 'font-lock-function-name-face nil :height 1.0)

;; change line number color (manoj-dark)
(after 'linum
  (set-face-attribute 'linum nil :foreground "gold"))

;; change mode-line's font size
(set-face-attribute 'mode-line nil :height 1.0)

;; default font
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-14"))

;; increase, decrease and adjust font size
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-0") #'text-scale-adjust)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(require-package 'vimish-fold)
(require 'vimish-fold)
(vimish-fold-global-mode t)

;; (require-package 'delight)

;; (delight '((auto-fill-mode nil t)
;;            (auto-revert-mode nil autorevert)
;;            (abbrev-mode nil abbrev)
;;            (whitespace-mode nil whitespace)
;;            (helm-mode nil helm-mode)
;;            (flyspell-mode nil flyspell)
;;            (projectile-mode nil projectile)
;;            (yas-minor-mode nil yasnippet)
;;            (undo-tree-mode nil undo-tree)
;;            (which-key-mode nil which-key)
;;            (company-mode nil company)
;;            (aggressive-indent-mode nil aggressive-indent)
;;            (evil-org-mode nil evil-org)
;;            (evil-vimish-fold-mode nil evil-vimish-fold)
;;            (eldoc-mode nil eldoc)
;;            (highlight-symbol-mode nil hightlight-symbol)
;;            ))

;; (delight 'server-buffer-clients nil 'server)
;; (delight 'auto-fill-function nil t)

(require-package 'diminish)
(require 'diminish)

(diminish 'visual-line-mode)
(diminish 'auto-fill-function)
(after 'whitespace
  (diminish 'global-whitespace-mode)
  (diminish 'whitespace-mode))
(after 'org-indent (diminish 'org-indent-mode))
(after 'aggressive-indent (diminish 'aggressive-indent-mode))
(after 'autorevert (diminish 'auto-revert-mode))
(after 'abbrev (diminish 'abbrev-mode))
(after 'subword (diminish 'subword-mode))
(after 'color-identifiers-mode (diminish 'color-identifiers-mode))
(after 'company (diminish 'company-mode))
(after 'counsel (diminish 'counsel-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'flycheck (diminish 'flycheck-mode))
(after 'flyspell (diminish 'flyspell-mode))
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
(after 'evil-org (diminish 'evil-org-mode))
(after 'evil-vimish-fold (diminish 'evil-vimish-fold-mode))
(after "intero-autoloads" (diminish 'intero-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(/boot/delayed-init
 (require-package 'color-identifiers-mode)
 (global-color-identifiers-mode)
 (diminish 'color-identifiers-mode))

(require-package 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'prog-mode-hook #'highlight-symbol-mode)

(require-package 'highlight-numbers)
(add-hook 'prog-mode-hook #'highlight-numbers-mode)

(require-package 'highlight-quoted)
(add-hook 'prog-mode-hook #'highlight-quoted-mode)

(require-package 'page-break-lines)
(global-page-break-lines-mode)

(provide 'init-eyecandy)
