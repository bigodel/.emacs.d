;;; config-evil.el --- Evil-mode config file

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(defvar dotemacs-evil/emacs-state-hooks
  '(org-log-buffer-setup-hook
    org-capture-mode-hook)
  "List of hooks to automatically start up in Evil Emacs state.")

(defvar dotemacs-evil/emacs-state-major-modes
  '(calculator-mode
    makey-key-mode
    dired-mode)
  "List of major modes that should default to Emacs state.")

(defvar dotemacs-evil/emacs-state-minor-modes
  '(magit-blame-mode)
  "List of minor modes that when active should switch to Emacs state.")

(defvar dotemacs-evil/insert-state-minor-modes
  '(git-commit-mode)
  "List of minor modes that when active should switch to Insert state.")

(defvar dotemacs-evil/insert-state-major-modes nil
  "List of major modes that when active should switch to Insert state.")

(defvar dotemacs-evil/emacs-insert-mode nil
  "If non-nil, insert mode will act as Emacs state.")

;; recenter after any jump in evil-mode
(add-hook 'evil-jumps-post-jump-hook #'recenter)

;; load evil
(require-package 'evil)
(require 'evil)
(evil-mode)

;; some configuration variables
(setvar evil-search-module 'isearch)
(setvar evil-magic 'very-magic)
(setvar evil-shift-width (symbol-value 'tab-width))
(setvar evil-regexp-search t)
(setvar evil-search-wrap t)
(setvar evil-want-C-i-jump t)
(setvar evil-want-C-u-scroll t)
(setvar evil-want-fine-undo nil)
(setvar evil-want-integration nil)
(setvar evil-want-keybinding nil)
(setvar evil-want-abbrev-expand-on-insert-exit nil)
;; move evil tag to beginning of modeline
(setvar evil-mode-line-format '(before . mode-line-front-space))

(setvar evil-emacs-state-cursor '("red" box))
(setvar evil-motion-state-cursor '("white" box))
(setvar evil-normal-state-cursor '("magenta" box))
(setvar evil-visual-state-cursor '("orange" box))
(setvar evil-insert-state-cursor '("red" bar))
(setvar evil-replace-state-cursor '("red" hbar))
(setvar evil-operator-state-cursor '("red" hollow))

;; emacs state in minor modes
(dolist (mode dotemacs-evil/emacs-state-minor-modes)
  (let ((hook (concat (symbol-name mode) "-hook")))
    (add-hook (intern hook) `(lambda ()
                               (if ,mode
                                   (evil-emacs-state)
                                 (evil-normal-state))))))

;; insert state in major modes
(dolist (mode dotemacs-evil/insert-state-minor-modes)
  (let ((hook (concat (symbol-name mode) "-hook")))
    (add-hook (intern hook) `(lambda ()
                              (if ,mode
                                  (evil-insert-state)
                                (evil-normal-state))))))

;; emacs state hooks
(dolist (hook dotemacs-evil/emacs-state-hooks)
  (add-hook hook #'evil-emacs-state))

;; emacs state in major modes
(dolist (mode dotemacs-evil/emacs-state-major-modes)
  (evil-set-initial-state mode 'emacs))

;; insert state in major modes
(dolist (mode dotemacs-evil/insert-state-major-modes)
  (evil-set-initial-state mode 'insert))

(after 'evil-common
  (evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
  (evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
  (evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
  (evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
  (evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
  (evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
  (evil-put-property 'evil-state-properties 'operator :tag " OPERATOR "))

(when dotemacs-evil/emacs-insert-mode
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state))

(unless (display-graphic-p)
  (evil-esc-mode 1))

(require-package 'evil-ediff)

(after 'magit
  (require-package 'evil-magit)
  (require 'magit)
  (evil-magit-init))

(after 'vimish-fold
  (require-package 'evil-vimish-fold)
  (evil-vimish-fold-mode t))

(require-package 'evil-matchit)
(global-evil-matchit-mode t)

(require-package 'evil-collection)
(require 'evil-collection)
(setvar evil-collection-setup-minibuffer nil)
(evil-collection-init)

(defadvice evil-ex-search-next (after dotemacs activate)
  "Recenter after going to the next result of a ex search."
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  "Recenter after going to the previous result of a ex search."
  (recenter))

(provide 'config-evil)
;;; config-evil.el ends here
