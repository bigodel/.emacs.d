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
(setvar 'ivy-use-virtual-buffers t) ; recentf & bookmarks in `ivy-switch-buffer'
(setvar 'ivy-count-format "%d/%d ")
(setvar 'ivy-wrap t)
(setvar 'ivy-height 16)
(setvar 'ivy-initial-inputs-alist nil)

;; `counsel' automatically uses `amx' as the M-x package
(setvar 'amx-history-length 10)
(setvar 'amx-save-file (expand-file-name "amx-items" dotemacs-cache-directory))
(require-package 'amx)
(amx-mode)

;; add actions to `counsel-find-file'
(ivy-set-actions
 'counsel-find-file
 '(("d" delete-file "delete")))

;; `counsel' wrapper for tramp
(require-package 'counsel-tramp)
(setvar 'counsel-tramp-custom-connections '(/doas:root@localhost:/))

;; make swiper faster
(after 'swiper
  (defadvice swiper (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum))
  (defadvice swiper-all (before dotemacs activate)
    (setq gc-cons-threshold most-positive-fixnum)))

;; a nice little hydra to work with ivy
(after 'hydra
  (require-package 'ivy-hydra))

(after 'projectile
  (require-package 'counsel-projectile)
  (counsel-projectile-mode t))

(provide 'config-ivy)
;;; config-ivy.el ends here
