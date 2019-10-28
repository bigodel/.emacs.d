;;; config-ivy.el --- Ivy configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; `counsel' requires as dependency `ivy' and `swiper'
(require-package 'counsel)

;; start `counsel' and `ivy'
(counsel-mode t)
(ivy-mode t)

;; variables
(setvar ivy-use-virtual-buffers t) ; recentf & bookmarks in `ivy-switch-buffer'
(setvar ivy-count-format "%d/%d ")
(setvar ivy-wrap t)
(setvar ivy-height 16)
(setvar ivy-initial-inputs-alist nil)

;; `counsel' automatically uses `amx' as the M-x package
(setvar amx-history-length 10)
(setvar amx-save-file (expand-file-name "amx-items" dotemacs-cache-directory))
(require-package 'amx)
(amx-mode)

;; add actions to `counsel-find-file'
(ivy-set-actions
 'counsel-find-file
 '(("d" delete-file "delete")))

;; `counsel' wrapper for tramp
(require-package 'counsel-tramp)
(/bindings/define-key (current-global-map) (kbd "C-c c t") #'counsel-tramp)

;; make swiper faster
(after 'swiper
  (defadvice swiper (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum))
  (defadvice swiper-all (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum)))

(after 'projectile
  (require-package 'counsel-projectile)
  (counsel-projectile-mode t))

;; ivy bindings
(/bindings/define-keys ivy-minibuffer-map
  ((kbd "M-m") #'ivy-mark)
  ((kbd "M-u") #'ivy-unmark))

(/bindings/define-keys (current-global-map)
  ((kbd "C-s") #'swiper)
  ((kbd "C-S") #'swiper-all)
  ((kbd "C-c C-r") #'ivy-resume "ivy-resume")
  ((kbd "M-x") #'counsel-M-x)
  ((kbd "C-x C-f") #'counsel-find-file)
  ((kbd "C-h S") #'counsel-info-lookup-symbol "lookup symbol")
  ((kbd "C-h u") #'counsel-unicode-char "unicode char")
  ((kbd "M-y") #'counsel-yank-pop)
  ((kbd "C-c c L") #'counsel-load-library "load library")
  ((kbd "C-c c P") #'counsel-package)
  ((kbd "C-c c f") #'counsel-find-library "find library")
  ((kbd "C-c c T") #'counsel-load-theme "load theme")
  ((kbd "C-c c h") #'counsel-command-history "command history")
  ((kbd "C-c c C") #'counsel-colors-emacs "colors emacs")
  ((kbd "C-c c c") #'counsel-colors-web "colors web")
  ((kbd "C-c c l") #'counsel-locate "locate")
  ((kbd "C-c c o") #'counsel-outline "jump to outline"))

;; try some searchers to see which to use; default to grep
(cond
 ((executable-find "rg")
  (/bindings/define-key (current-global-map)
    (kbd "C-c c g") #'counsel-rg))

 ((executable-find "ag")
  (/bindings/define-key (current-global-map)
    (kbd "C-c c g") #'counsel-ag))

 ((executable-find "pt")
  (/bindings/define-key (current-global-map)
    (kbd "C-c c g") #'counsel-pt))

 (t
  (/bindings/define-key (current-global-map)
    (kbd "C-c c g") #'counsel-grep)))

(after 'yasnippet
  (/bindings/define-key (current-global-map)
    (kbd "C-c c y") #'ivy-yasnippet))

(provide 'config-ivy)
;;; config-ivy.el ends here
