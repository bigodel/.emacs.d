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
(load-theme 'manoj-dark t)

;; make fringe same background color as line-number face
(set-face-background 'fringe (face-background 'line-number))

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
(add-to-list 'default-frame-alist '(font . "monospace-14"))

;;; line numbers (only available in Emacs 26+)
(defconst dotemacs-eyecandy/line-numbers-disabled-hooks
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
  (setvar display-line-numbers nil)
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

;; hide all minor modes from mode line
(require-package 'rich-minority)
(unless rich-minority-mode
  (rich-minority-mode t))
(setf rm-blacklist "")

;; TODO: folding

;; beautiful lines instead of ^L
(require-package 'page-break-lines)
(global-page-break-lines-mode)

(provide 'config-eyecandy)
;;; config-eyecandy.el ends here
