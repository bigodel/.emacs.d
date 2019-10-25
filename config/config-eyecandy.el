;;; config-eyecandy.el --- Some eyecandy to make things more beautiful

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;; color theme
;; install solarized theme
;; (require-package 'solarized-theme)

;; (setvar solarized-distinct-fringe-background nil) ; make the fringe stand out
;; (setvar solarized-distinct-doc-face t)            ; make documentation stand out
;; (setvar solarized-use-variable-pitch t)           ; all fonts same size
;; (setvar solarized-high-contrast-mode-line t)      ; high contrast mode line
;; (setvar solarized-use-more-italic t)              ; use more italics
;; (setvar solarized-scale-org-headlines t) ; don't change size of org-mode

;; solarized adds line spacing, I don't want it
;; (setvar line-spacing nil)

;; avoid all font-size changes
;; (setvar solarized-height-minus-1 1.0)
;; (setvar solarized-height-plus-1 1.0)
;; (setvar solarized-height-plus-2 1.0)
;; (setvar solarized-height-plus-3 1.0)
;; (setvar solarized-height-plus-4 1.0)

;; which func should be blue (solarized)
;; (after 'which-func
;;   (set-face-attribute 'which-func nil :foreground "midnight blue"))

(load-theme 'manoj-dark t)

;; darker solarized background
;; (set-face-background 'default "#00181e")

;; line numbers config
;; (set-face-attribute 'line-number nil :background "grey8" :foreground "white")
;; (set-face-attribute 'line-number-current-line nil :weight 'bold)

;; change fringe background and foreground color
(set-face-background 'fringe (face-background 'line-number))
;; (set-face-foreground 'fringe (face-foreground 'line-number))

;; disable the bigger scale on bold function fonts (manoj-dark)
(set-face-attribute 'font-lock-function-name-face nil :height 1.0)

;; make comments grey (manoj-dark and default)
(set-face-foreground 'font-lock-comment-face "dimgray")
(set-face-foreground 'font-lock-comment-delimiter-face "dimgray")

;; change mode-line's face (manoj-dark)
(set-face-attribute 'mode-line nil :height 1.0 :underline nil)
(set-face-attribute 'mode-line-buffer-id nil :height 1.0)
(set-face-attribute 'mode-line-inactive nil :underline nil)

;; default font
;; (add-to-list 'default-frame-alist '(font . "monospace-14"))

;;; line numbers (only available in Emacs 26+)
(defvar dotemacs-eyecandy/line-numbers-disabled-hooks
  '(eshell-mode-hook
    woman-mode-hook
    man-mode-hook
    helpful-mode-hook
    help-mode-hook
    treemacs-mode-hook
    dired-mode-hook
    doc-view-mode-hook
    pdf-view-mode-hook
    proof-goals-mode-hook
    proof-response-mode-hook)
  "Modes to disable `display-line-numbers-mode'.")

(when (fboundp 'display-line-numbers-mode)
  (setvar display-line-numbers t)
  (setvar display-line-numbers-current-absolute t)

  (dolist (hook dotemacs-eyecandy/line-numbers-disabled-hooks)
    (add-hook hook (lambda ()
                     "Disable `display-line-numbers-mode'."
                     (display-line-numbers-mode -1)))))

;;; whitespace-mode
(setvar whitespace-style
        '(face trailing tabs tab-mark lines-tail))

(add-hook 'after-save-hook #'whitespace-cleanup)

;; I don't enable `global-whitespace-mode' because there are non file modes,
;; like `dired', in which I don't want it activated
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'whitespace-mode)

(after 'whitespace
  (set-face-attribute 'whitespace-trailing nil
                      :foreground (face-foreground 'default)
                      :background "gray15"))

;;; misc
;; stop blinking cursor
(blink-cursor-mode -1)

;; modeline indicators
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(which-function-mode)

;; diminish modes from the mode line
;; (require-package 'diminish)
;; (require 'diminish)

;; (diminish 'visual-line-mode)
;; (diminish 'auto-fill-function)
;; (after 'whitespace
;;   (diminish 'global-whitespace-mode)
;;   (diminish 'whitespace-mode))
;; (after 'org-indent (diminish 'org-indent-mode))
;; (after 'aggressive-indent (diminish 'aggressive-indent-mode))
;; (after 'autorevert (diminish 'auto-revert-mode))
;; (after 'abbrev (diminish 'abbrev-mode))
;; (after 'color-identifiers-mode (diminish 'color-identifiers-mode))
;; (after 'company (diminish 'company-mode))
;; (after 'ivy (diminish 'ivy-mode))
;; (after 'counsel (diminish 'counsel-mode))
;; (after 'eldoc (diminish 'eldoc-mode))
;; (after 'flycheck (diminish 'flycheck-mode))
;; (after 'flyspell (diminish 'flyspell-mode))
;; (after 'highlight-symbol (diminish 'highlight-symbol-mode))
;; (after 'projectile (diminish 'projectile-mode))
;; (after 'undo-tree (diminish 'undo-tree-mode))
;; (after 'which-key (diminish 'which-key-mode))
;; (after 'yasnippet (diminish 'yas-minor-mode))
;; (after 'evil-org (diminish 'evil-org-mode))
;; (after 'evil-vimish-fold (diminish 'evil-vimish-fold-mode))
;; (after 'proof-site (diminish 'proof-active-buffer-fake-minor-mode))
;; (after 'color-identifiers (diminish 'color-identifiers-mode))
;; (after 'page-break-lines (diminish 'page-break-lines-mode))

;; hide all minor modes from mode line
(require-package 'rich-minority)
(rich-minority-mode t)
(setf rm-blacklist "")

;; TODO: folding

;; beautiful lines instead of ^L
(require-package 'page-break-lines)
(global-page-break-lines-mode)

(provide 'config-eyecandy)
;;; config-eyecandy.el ends here
