;;; config-misc.el --- Miscellaneous configuration -*- lexical-bindings: t; -*-

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
(disable-mouse-mode -1)
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

;;; better window management with `ace-window'
(require-package 'ace-window)

;;; `treemacs'
(require-package 'treemacs)

(after [treemacs projectile]
  (require-package 'treemacs-projectile))

(after [treemacs magit]
  (require-package 'treemacs-magit))

(setvar 'treemacs-persist-file               ; location of persist file
        (concat dotemacs-cache-directory "treemacs-persist"))
(setvar 'treemacs-indentation 2)        ; number of spaces for indentation
;; (setvar 'treemacs-indentation-string    ; the string to show on indent level
;;         (propertize "|" 'face 'font-lock-comment-face))
(setvar 'treemacs-width 40)             ; default width of the treemacs window
                                        ; TODO: modify treemacs faces so the
                                        ; fonts are smaller
;; some option only behave well if python3 is installed
(setvar 'treemacs-collapse-dirs (if (executable-find "python3") 3 0))
(setvar 'treemacs-project-follow-cleanup t) ; close projects and expand current
(setvar 'treemacs-follow-after-init t)      ; focus the current file when open

(after 'treemacs
  (treemacs-fringe-indicator-mode -1)   ; don't show the fringe helper
  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-filewatch-mode t)
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

;;; better help
(require-package 'helpful)
(after [evil helpful]
  (evil-set-initial-state 'helpful-mode 'motion))

;;; hungry delete
;; NOTE: this isn't needed for modes that have `aggressive-indent-mode' on, but
;; i don't have it enabled globally so this helps (it might make me not use
;; `aggressive-indent-mode' anymore...). also the full configuration depends on
;; that is present in `bindings-misc'.
;; this package basically adds the functionality of `hungry-delete-mode' that is
;; available only for `cc-mode' everywhere you might want it
(require-package 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)

;;; automatically insert and manage parenthesis
;; TODO: look into smartparens config and the strict mode
(require-package 'smartparens)
(add-hook 'after-init-hook #'smartparens-global-mode)
;; since we don't have a elisp file for configuration, we do it here
(after 'smartparens
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

;;; search from within emacs
(require-package 'engine-mode)
(setvar 'engine/keybinding-prefix "C-x /")
(defengine duckduckgo
  "https://duckduckgo.com/?q=%s" :keybinding "d")
(engine-mode t)

(provide 'config-misc)
;;; config-misc.el ends here
