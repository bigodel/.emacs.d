;;; init.el --- Main file

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; Most (if not all,) of this configuration was inspired/copied from Bailey
;; Ling's Emacs configuration, found on https://github.com/bling/dotemacs
;;
;; TODO: change all defvar's to defconst's
;; TODO: check if whithout the provide's it makes it faster
;;; Code:
;; some common lisp functions
(eval-when-compile (require 'cl))

;;; log Emacs startup time in *Messages*
(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time)
                                                        emacs-start-time))))
                (message "[Emacs initialized in %.3fs]" elapsed)))))

;;; let
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

  ;;; constants
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

  ;;; packages
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
  ;; activate installed packages when Emacs starts
  ;; we set this to nil to stop Emacs from starting it twice
  (setq package-enable-at-startup nil)
  ;; start the package system
  (package-initialize)

  ;;; load some core stuff
  (load (concat core-directory "core-boot"))
  ;; temporary -------
  (load (concat core-directory "core-util"))
  ;; TODO: change all the instances of /bindings/ to bindings- and remove this
  (load (concat core-directory "core-bindings"))
  ;; temporary -------

  ;;; set and load custom file
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;;; load all of our configuration files
  ;; the bindings configuration needs to get loaded before every thing else,
  ;; since it has the definitions for the macros of defining keys and what not.
  (cl-loop for file in (append (reverse (directory-files-recursively
                                         config-directory "\\.el\\'"))
                               (reverse (directory-files-recursively
                                         bindings-directory "\\.el$")))
           do (condition-case ex
                  (load (file-name-sans-extension file))
                ('error
                 (with-current-buffer "*scratch*"
                   (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex)))))))

(provide 'init)
;;; init.el ends here
