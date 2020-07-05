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
  (require-package 'undo-tree)
  (setvar 'undo-tree-auto-save-history t)
  (setvar 'undo-tree-enable-undo-in-region nil)
  (setvar 'undo-tree-history-directory-alist
          `(("." . ,(expand-file-name "undo/" dotemacs-cache-directory))))
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
        (expand-file-name "treemacs-persist" dotemacs-cache-directory))
(setvar 'treemacs-indentation 2)        ; number of spaces for indentation
;; (setvar 'treemacs-indentation-string    ; the string to show on indent level
;;         (propertize "|" 'face 'font-lock-comment-face))
(setvar 'treemacs-width 45)             ; default width of the treemacs window
(setvar 'treemacs--width-is-locked nil) ; let us resize
;; some option only behave well if python3 is installed
(setvar 'treemacs-collapse-dirs (if (executable-find "python3") 3 0))
(setvar 'treemacs-project-follow-cleanup t) ; close projects and expand current
(setvar 'treemacs-follow-after-init t)      ; focus the current file when open
(setvar 'treemacs-is-never-other-window nil) ; `other-window' goes to treemacs

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

;;; automatically insert and manage parenthesis
;; (require-package 'smartparens)
;; (add-hook 'after-init-hook #'smartparens-global-mode)
;; ;; since we don't have a elisp file for configuration, we do it here
;; (after 'smartparens
;;   (sp-local-pair 'emacs-lisp-mode "`" "'")
;;   (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))
(electric-pair-mode t)
(defmacro misc-add-electric-pairs (hook pairs)
  "Add PAIRS on HOOK to `electric-pair-pairs'.
If HOOK is nil, make it a new global pair."
  (after 'electric
    `(if ,hook
         (add-hook
          ,hook
          (lambda ()
            (setvar 'electric-pair-pairs
                    (append electric-pair-pairs ,pairs) 'local)
            (setvar 'electric-pair-text-pairs electric-pair-pairs 'local)))
       (setvar 'electric-pair-pairs (append electric-pair-pairs ,pairs))
       (setvar 'electric-pair-text-pairs electric-pair-pairs))))

;; add pairs to org mode
(misc-add-electric-pairs 'org-mode-hook '((?` . ?')))


;;; jump to definitions without TAGS
(require-package 'dumb-jump)
(after 'ivy
  (setvar 'dumb-jump-selector 'ivy))
(cond
 ((executable-find "rg")
  (setvar 'dumb-jump-prefer-searcher 'rg))
 ((executable-find "ag")
  (setvar 'dumb-jump-prefer-searcher 'ag))
 (t
  (setvar 'dumb-jump-prefer-searcher 'grep)))
;; make dumb-jumps also evil jumps
(after [evil dumb-jump]
  (defadvice dumb-jump-go (before dotemacs activate)
    (evil-set-jump)))

;;; pomodoro timer
(require-package 'pomidor)
(setvar 'pomidor-sound-tick nil)
(setvar 'pomidor-sound-tack nil)
(setvar 'alert-default-style 'libnotify) ; `alert' is a dep of `pomidor'

(provide 'config-misc)
;;; config-misc.el ends here
