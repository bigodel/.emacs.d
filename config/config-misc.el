;;; config-org.el --- Org mode configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; disable mouse
(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter "NoMouse!"
  :keymap (make-sparse-keymap))

;; bind all mouse related keys to ignore
(dolist (type '(mouse down-mouse drag-mouse double-mouse triple-mouse))
  (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
    (dotimes (n 7)
      (let ((k (format "%s%s-%s" prefix type n)))
        (define-key disable-mouse-mode-map
          (vector (intern k)) #'ignore)))))

;; enable the new disable mouse minor mode
(disable-mouse-mode t)
;; needed for the evil states
(after 'evil
  (evil-make-overriding-map disable-mouse-mode-map))

;;; undo like vim (only needed if using evil-mode)
(after 'evil
  (package-install 'undo-tree)
  (setvar 'undo-tree-auto-save-history t)
  (setvar 'undo-tree-enable-undo-in-region nil)
  (setvar 'undo-tree-history-directory-alist
          `(("." . ,(concat dotemacs-cache-directory "undo/"))))
  (setvar 'undo-tree-visualizer-timestamps t)
  (setvar 'undo-tree-visualizer-diff t)
  (add-hook 'after-init-hook #'global-undo-tree-mode))

;;; `treemacs'
(require-package 'treemacs)

(after [treemacs projectile]
  (require-package 'treemacs-projectile))

(after [treemacs magit]
  (require-pacakge 'treemacs-magit))

(setvar 'treemacs-persist-file               ; location of persist file
        (concat dotemacs-cache-directory "treemacs-persist"))
(setvar 'treemacs-indentation-string    ; show this char as and indent guide
        (propertize "│  " 'face 'font-lock-comment-face))
(setvar 'treemacs-indentation 1)        ; number of spaces for indentation
;; some option only behave well if python3 is installed
(setvar 'treemacs-collapse-dirs (if (executable-find "python3") 3 0))

(after 'treemacs
  (treemacs-fringe-indicator-mode -1)   ; don't show the fringe helper
  ;; if git and python3 are installed, use deferred, otherwise use simple
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

;;; `doc-view-mode'
(setvar 'doc-view-continuous t)          ; continuous mode in `doc-view-mode'
(setvar 'doc-view-resolution 200)        ; resolution of images

;;; `pdf-view-mode'
(require-package 'pdf-tools)
;; variables
(setvar 'pdf-view-resize-factor 1.1)
;; midnight mode colors for `pdf-view-mode'
(setvar 'pdf-view-midnight-colors '("#ffffff" . "#000000")) ;
;; enable some pdf-view related minor-modes
(add-hook 'pdf-tools-enabled-hook #'pdf-view-midnight-minor-mode)
(add-hook 'pdf-tools-enabled-hook #'pdf-view-auto-slice-minor-mode)
(add-hook 'pdf-tools-enabled-hook #'pdf-view-printer-minor-mode)
(add-hook 'doc-view-mode-hook
          (lambda ()
            "Lambda function to use `pdf-tools' in
`doc-view-mode' when in a pdf file."
            (require 'pdf-tools)
            (pdf-tools-install)
            (pdf-view-mode)))

;;; aggressive indent
(defconst dotemacs-misc-aggressive-indent-hooks
  '(c-mode-hook
    cc-mode-hook
    lisp-mode-hook
    emacs-lisp-mode-hook
    sh-mode-hook
    java-mode-hook
    python-mode-hook
    proof-mode-hook)
  "Hooks for major modes to activate `aggressive-indent-mode'.")

(require-package 'aggressive-indent)
(dolist (hook dotemacs-misc-aggressive-indent-hooks)
  (add-hook hook #'aggressive-indent-mode))

;;; better help
(require-package 'helpful)
(after [evil helpful]
  (evil-set-initial-state 'helpful-mode 'motion))

(provide 'config-misc)
;;; config-misc.el ends here
