;;; init.el --- Main file

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; some common lisp functions
(eval-when-compile (require 'cl))

;; log Emacs startup time in *Messages*
(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time)
                                                        emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;; we wrap our whole initializatino inside this let because we set some
;; variables to make it snappier and faster to start Emacs. also, we don't need
;; a variable to store the location of our config outside of the initialization,
;; that's why it is also inside the let.
;; TODO: write about the gc-cons-threshold and file-name-handler-alist!
(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (core-directory (concat user-emacs-directory "core/"))
      (bindings-directory (concat user-emacs-directory "bindings/"))
      (config-directory (concat user-emacs-directory "config/")))

  ;; disable bars to have a as clean as possible interface
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  (defconst dotemacs-globally-ignored-directories
    '("elpa" ".cache" "target" "dist" "node_modules" ".git" ".hg" ".svn"
      ".idea")
    "A set of default directories to ignore for anything that
involves searching.")

  (defconst dotemacs-cache-directory
    (concat user-emacs-directory ".cache/")
    "The storage location for various persistent files.")

  ;; check if the cache dir exists, if not ask to create it
  (when (and (not (file-directory-p dotemacs-cache-directory))
             (y-or-n-p
              (format "Directory `%s' does not exist! Create it?"
                      dotemacs-cache-directory)))
    (make-directory dotemacs-cache-directory t))

  ;; packages
  (setq package-archives
        '(("melpa-stable" . "https://stable.melpa.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ("gnu" . "http://elpa.gnu.org/packages/")))

  ;; set the priorities when installing packages
  (setq package-archive-priorities
        '(("melpa-stable" . 4)
          ("melpa" . 3)
          ("org" . 2)
          ("gnu" . 1)))

  (setq package-enable-at-startup nil)
  (package-initialize)
  ;; initialize packages
  ;; (when (eval-when-compile (version< emacs-version "27"))
  ;;   (load "~/.emacs.d/early-init.el")
  ;;   (package-initialize))
  )

;;; load path
;; force "config/core" and "config" at the head to reduce the startup time.
;; (defun update-load-path (&rest _)
;;   "Update `load-path'."
;;   (push (expand-file-name "config" user-emacs-directory) load-path)
(push (expand-file-name "core" user-emacs-directory) load-path)
;;
;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'."
;;   (let ((default-directory
;;           (expand-file-name "config" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))
;;
;; (advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)
;;
;; (update-load-path)

;;; core
;; this should be run before any of the configuration files. it has stuff used
;; in a lot of different files.
;; load some core macros and advices
(load "core-boot")
;; load some useful functions
(load "core-util")
;; load some bindings configurations
(load "core-bindings")

;; set the file for storing customization information
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; recursively load each config file
(dolist (file (reverse (directory-files-recursively
                        (concat user-emacs-directory "config") "\\.el$")))
  ;; (load (file-name-sans-extension file)))
  (condition-case err
      (load (file-name-sans-extension file))
    ('error (with-current-buffer "*scratch*"
              (insert (format "[INIT ERROR]\n%s\n%s\n\n" file err))))))

;; decide the best order to load files
;; (load "config-basic")
;; (load "config-eyecandy")
;; (load "config-eshell")
;; (load "config-org")
;; (load "config-ivy")
;; (load "config-evil")
;; (load "config-yasnippet")
;; (load "config-company")
;; (load "config-projectile")
;; (load "config-vcs")
;; (load "config-misc")
;; (load "lang-pg")
;; (load "lang-tex")

;; try to do this but with require
;; (dolist (file (reverse (directory-files-recursively
;;                         (concat user-emacs-directory "config") "\\.el$")))
;;   (condition-case err
;;       (require nil (file-relative-name file dotemacs-config-directory))
;;     ('error (with-current-buffer "*scratch*"
;;               (insert (format "[INIT ERROR]\n%s\n%s\n\n" file err))))))

;; disable debug on error (it gets annoying)
(setq debug-on-error nil)

(provide 'init)
;;; init.el ends here
