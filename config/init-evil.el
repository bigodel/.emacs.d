(defvar dotemacs-evil/emacs-state-hooks
  '(org-log-buffer-setup-hook
    org-capture-mode-hook)
  "List of hooks to automatically start up in Evil Emacs state.")

(defvar dotemacs-evil/emacs-state-major-modes
  '(calculator-mode
    makey-key-mode)
  "List of major modes that should default to Emacs state.")

(defvar dotemacs-evil/emacs-state-minor-modes
  '(git-commit-mode
    magit-blame-mode)
  "List of minor modes that when active should switch to Emacs state.")

(defvar dotemacs-evil/emacs-insert-mode nil
  "If non-nil, insert mode will act as Emacs state.")

(setq evil-search-module 'isearch-regexp)
(setq evil-magic 'very-magic)
(setq evil-shift-width (symbol-value 'tab-width))
(setq evil-regexp-search t)
(setq evil-search-wrap t)
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)
(setq evil-want-fine-undo nil)
(setq evil-want-integration nil)
(setq evil-want-abbrev-on-insert-exit nil)
(setq evil-want-abbrev-expand-on-insert-exit nil)
;; move evil tag to beginning of modeline
(setq evil-mode-line-format '(before . mode-line-front-space))

(setq evil-emacs-state-cursor '("red" box))
(setq evil-motion-state-cursor '("white" box))
(setq evil-normal-state-cursor '("magenta" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" hbar))
(setq evil-operator-state-cursor '("red" hollow))

(add-hook 'evil-jumps-post-jump-hook #'recenter)

(require-package 'evil)
(require 'evil)
(evil-mode)

(cl-loop for mode in dotemacs-evil/emacs-state-minor-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) (lambda ()
                                        (if ,mode
                                            (evil-emacs-state)
                                          (evil-normal-state))))))

(cl-loop for hook in dotemacs-evil/emacs-state-hooks
         do (add-hook hook #'evil-emacs-state))

(cl-loop for mode in dotemacs-evil/emacs-state-major-modes
         do (evil-set-initial-state mode 'emacs))

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

(require-package 'evil-surround)
(global-evil-surround-mode t)

(after 'magit
  (require-package 'evil-magit)
  (require 'evil-magit)
  (evil-magit-init))

(after 'org
  (require-package 'evil-org)
  (require 'evil-org)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(after 'vimish-fold
  (require-package 'evil-vimish-fold)
  (require 'evil-vimish-fold)
  (evil-vimish-fold-mode t))

  (require-package 'evil-matchit)
  (defun evilmi-customize-keybinding ()
    (evil-define-key 'normal evil-matchit-mode-map
      "%" 'evilmi-jump-items))
  (global-evil-matchit-mode t)

  (require-package 'evil-indent-textobject)
  (require 'evil-indent-textobject)

  (require-package 'evil-visualstar)
  (global-evil-visualstar-mode t)

  (require-package 'evil-numbers)

(after 'evil
  (require-package 'evil-collection)
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-company-use-tng t)
  (evil-collection-init))

(defadvice evil-ex-search-next (after dotemacs activate)
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  (recenter))

(provide 'init-evil)
