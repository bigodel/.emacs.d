;;; init.el --- Main file

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; enter debugger when an error is encountered
(setq debug-on-error t)

(eval-when-compile (require 'cl))

;; log Emacs startup time in *Messages*
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs startup time: %s" (emacs-init-time))))

(let ((gc-cons-threshold (* 512 1024 1024))
      (file-name-handler-alist nil)
      (config-directory (concat user-emacs-directory "config/"))
      (core-directory (concat user-emacs-directory "config/core/")))

  ;; define the cache directory
  (defvar dotemacs-cache-directory (concat user-emacs-directory ".cache/")
    "The storage location for various persistent files.")

  ;; check if the cache dir exists, if not ask to create it
  (when (and (not (file-directory-p dotemacs-cache-directory))
             (y-or-n-p
              (format "Directory `%s' does not exist! Create it?"
                      dotemacs-cache-directory)))
    (make-directory dotemacs-cache-directory t))

  (defvar dotemacs-globally-ignored-directories
    '("elpa" ".cache" "target" "dist" "node_modules" ".git" ".hg" ".svn" ".idea")
    "A set of default directories to ignore for anything that
    involves searching.")

  (require 'package)

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

  ;; configure Emacs package manager
  ;; not required on Emacs 27
  (when (version< emacs-version "27")
    (package-initialize))

  ;; load some core macros and advices
  (load (concat core-directory "core-boot"))

  ;; set the file for storing customization information
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; recursively load each config file
  (dolist (file (reverse (directory-files-recursively
                          "~/.emacs.d/config" "\\.el$")))
    (condition-case ex
        (load (file-name-sans-extension file))
      ('error (with-current-buffer "*scratch*"
                (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex)))))
    (load (file-name-sans-extension file))))

(provide 'init.el)
;;; init.el ends here
