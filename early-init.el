;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

;;; Commentary:
;;
;; Emacs 27+ introduces `early-init.el', which is run before init.el, before
;; package and UI initialization happens.
;;
;; Remember to remove the modifications made here in the `init.el' and other
;; config files.
;;
;;; Code:
;; defer garbage collection further back in the startup process
(setq gc-cons-threshold (* 512 1024 1024))

;; TODO: write why this is here
(setq file-name-handler-alist nil)

;; package initialize occurs automatically, before `user-init-file' is loaded,
;; but after `early-init-file'. I handle package initialization, so we must
;; prevent Emacs from doing it early!
;; TODO: review the need of this
(setq package-enable-at-startup nil)

;; faster to disable these here (before they've been initialized)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(modify-all-frames-parameters '((vertical-scroll-bars)))

(add-hook 'after-init-hook
          (lambda ()
            "Revert some variables to their original value.
The varialbes `gc-cons-threshold' and `file-name-handler-alist'
have their values altered in in `~/.emacs.d/early-init.el' for
faster startup."
            (setq gc-cons-threshold 800000)
            (setq file-name-handler-alist t)))

;;; early-init.el ends here
(provide 'early-init)
