;;; config-evil.el --- Evil-mode config file

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; constants
(defconst dotemacs-evil/emacs-state-hooks
  '(org-log-buffer-setup-hook
    org-capture-mode-hook)
  "List of hooks to automatically start up in Evil Emacs state.")

(defconst dotemacs-evil/emacs-state-major-modes
  '(ibuffer-mode
    bookmark-bmenu-mode
    calculator-mode
    makey-key-mode
    dired-mode
    compilation-mode
    package-menu-mode
    treemacs-mode)
  "List of major modes that should default to Emacs state.")

(defconst dotemacs-evil/emacs-state-minor-modes
  '(magit-blame-mode)
  "List of minor modes that when active should switch to Emacs state.")

(defconst dotemacs-evil/insert-state-major-modes
  '(git-commit-mode)
  "List of minor modes that when active should switch to Insert state.")

;;; variables
(setvar evil-search-module 'evil-search) ; emulate vim's search
(setvar evil-magic 'very-magic)          ; vim's magicness (for 'evil-search)
(setvar evil-shift-width 4)              ; offset of < and >
(setvar evil-regexp-search t)            ; whether to use regexp for search
(setvar evil-search-wrap t)              ; whether searchs wraps around
(setvar evil-want-C-i-jump t)            ; whether C-i works like in vim
(setvar evil-want-C-u-scroll t)          ; whether C-u works like in vim
(setvar evil-want-fine-undo nil)         ; whether to undo like vim or emacs
(setvar evil-want-integration t)         ; whether to load evil-integration.el
(setvar evil-want-keybinding nil)        ; whether to load evil-keybindings.el
(setvar evil-want-abbrev-expand-on-insert-exit nil) ; expand abbrev with ESC
(setvar evil-vsplit-window-right t)     ; vsplit creates window to the right
(setvar evil-split-window-below t)      ; split creates window below
(setvar evil-ex-search-vim-style-regexp t) ; vim-style \ codes are supported in
                                        ; search pattern (only has affectes if
                                        ; evil-search-module is 'evil-search
;; move evil tag to beginning of modeline
(setvar evil-mode-line-format '(before . mode-line-front-space))
;; set the cursor for each state
(setvar evil-emacs-state-cursor    '("red" box))
(setvar evil-motion-state-cursor   '("white" box))
(setvar evil-normal-state-cursor   '("magenta" box))
(setvar evil-visual-state-cursor   '("orange" box))
(setvar evil-insert-state-cursor   '("red" bar))
(setvar evil-replace-state-cursor  '("red" hbar))
(setvar evil-operator-state-cursor '("magenta" hollow))

;; recenter after any jump in evil-mode
(add-hook 'evil-jumps-post-jump-hook #'recenter)

;; load `evil'
(require-package 'evil)
(evil-mode 1)

;; emacs state hooks
(cl-loop for hook in dotemacs-evil/emacs-state-hooks
         do (add-hook hook #'evil-emacs-state))

;; emacs state in major modes
(cl-loop for mode in dotemacs-evil/emacs-state-major-modes
         do (evil-set-initial-state mode 'emacs))

;; emacs state in minor modes
(cl-loop for mode in dotemacs-evil/emacs-state-minor-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) `(lambda ()
                                         "Start minor mode in Emacs state."
                                         (if ,mode
                                             (evil-emacs-state)
                                           (evil-normal-state))))))
;; insert state in major modes
(cl-loop for mode in dotemacs-evil/insert-state-major-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) `(lambda ()
                                         "Start mode in insert state."
                                         (if ,mode
                                             (evil-insert-state)
                                           (evil-normal-state))))))

;; change the modeline tag for each state
(evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
(evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
(evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
(evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
(evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
(evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
(evil-put-property 'evil-state-properties 'operator :tag " OPERATOR ")

(unless (display-graphic-p)
  (evil-esc-mode 1))

(after 'org
  (require-package 'evil-org)
  (add-hook 'org-mode-hook 'evil-org-mode)
  (after 'evil-org
    (evil-org-set-key-theme)
    (after 'evil-org-agenda
      (evil-org-agenda-set-keys))))

;; match tags like <b> in html or \begin{env} in LaTeX
(require-package 'evil-matchit)
(global-evil-matchit-mode t)

(defadvice evil-ex-search-next (after dotemacs activate)
  "Recenter after going to the next result of a ex search."
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  "Recenter after going to the previous result of a ex search."
  (recenter))

;; bindings
(defun evil-mouse-yank-primary ()
  "Insert the contents of primary selection into the location of
point."
  (interactive)
  (let ((default-mouse-yank-at-point (symbol-value mouse-yank-at-point)))
    (setvar mouse-yank-at-point t)
    (mouse-yank-primary nil)
    (setvar mouse-yank-at-point default-mouse-yank-at-point)))

(evil-define-key '(insert normal visual) 'global
  (kbd "<S-insert>") #'evil-mouse-yank-primary)

(provide 'config-evil)
;;; config-evil.el ends here
