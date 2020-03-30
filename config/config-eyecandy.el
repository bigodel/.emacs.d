;;; config-eyecandy.el --- Some eyecandy to make things more beautiful

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; disable bars to have a as clean as possible interface
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (unless (display-graphic-p) (menu-bar-mode -1)) ; enable in GUI Emacs
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;; color theme
;; use wombat
(load-theme 'wombat t)

;; make fringe same background color as line-number face
(when (version<= "26" emacs-version)
  (set-face-background 'fringe (face-background 'line-number)))

;; disable the bigger scale on bold function fonts (manoj-dark)
;; (set-face-attribute 'font-lock-function-name-face nil :height 1.0)

;; make comments grey (manoj-dark and default)
;; (set-face-foreground 'font-lock-comment-face "dimgray")
;; (set-face-foreground 'font-lock-comment-delimiter-face "dimgray")

;; change mode-line's face (manoj-dark)
;; (set-face-attribute 'mode-line nil :height 1.0 :underline nil) ;
;; (set-face-attribute 'mode-line-buffer-id nil :height 1.0)
;; (set-face-attribute 'mode-line-inactive nil :underline nil)

;; a custom theme to run on top of the other custom themes loaded (so it should
;; be here, after (load-theme 'blah)) that shows the name of the host when in
;; using tramp in the modeline alongside the buffer name. see
;; `tramp-theme-face-remapping-alist' for customization options
(require-package 'tramp-theme)
(load-theme 'tramp t)

;;; default font
(set-frame-font "monospace-13" nil t)

;;; line numbers (only available in Emacs 26+)
(defconst eyecandy-line-numbers-disabled-hooks
  '(eshell-mode-hook
    woman-mode-hook
    man-mode-hook
    helpful-mode-hook
    help-mode-hook
    treemacs-mode-hook
    dired-mode-hook
    doc-view-mode-hook
    pdf-view-mode-hook
    lsp-ui-doc-frame-mode-hook
    lsp-ui-imenu-mode-hook
    proof-goals-mode-hook
    proof-response-mode-hook)
  "Modes to disable `display-line-numbers-mode'.")

(when (fboundp 'display-line-numbers-mode)
  (setvar 'display-line-numbers t)
  (setvar 'display-line-numbers-current-absolute t)

  (dolist (hook eyecandy-line-numbers-disabled-hooks)
    (add-hook hook (lambda ()
                     "Disable `display-line-numbers-mode'."
                     (display-line-numbers-mode -1)))))

;;; whitespace-mode
(setvar 'whitespace-style
        '(face trailing tabs tab-mark lines-tail))

(add-hook 'after-save-hook #'whitespace-cleanup)

;; I don't enable `global-whitespace-mode' because there are non file modes,
;; like `dired', in which I don't want it activated
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'whitespace-mode)

;;; misc
;; if using git, strip the backend string from the mode line
(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git" " " vc-mode))))

;; stop blinking cursor
(blink-cursor-mode -1)

;; modeline indicators
(line-number-mode)
(column-number-mode)
(size-indication-mode)
(which-function-mode)

(after 'which-func                      ; i don't like the bright blue
  (set-face-foreground 'which-func (face-foreground 'default)))

;; hide all minor modes from mode line (not needed with doom-modeline)
(require-package 'rich-minority)
(unless rich-minority-mode
  (rich-minority-mode t))
(setvar 'rm-whitelist "FlyC")

;; highlight TODO
(require-package 'hl-todo)
(global-hl-todo-mode t)

;; beautiful lines instead of ^L
(require-package 'page-break-lines)
(global-page-break-lines-mode)

(provide 'config-eyecandy)
;;; config-eyecandy.el ends here
