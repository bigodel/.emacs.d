
;; turn this back on
(setq debug-on-error nil)

(eval-when-compile (require 'cl))

;; Log Emacs startup time in *Messages*
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs startup time: %s" (emacs-init-time))))

(let ((gc-cons-threshold (* 512 1024 1024))
      (file-name-handler-alist nil)
      (config-directory (concat user-emacs-directory "config/")))

(defvar dotemacs-cache-directory (concat user-emacs-directory ".cache/")
  "The storage location for various persistent files.")

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
        ("melpa" .  3)
        ("org" . 2)
        ("gnu" . 1)))

(setq package-enable-at-startup nil)

;; configure Emacs package manager
;; not required on Emacs 27
(when (version< emacs-version "27")
  (package-initialize))

(load (concat config-directory "init-boot"))

  (require 'ob-tangle)
  (org-babel-tangle-file (concat user-emacs-directory "init.el"))

  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (cl-loop for file in (reverse (directory-files-recursively
                                 config-directory "\\.el$"))
           do (condition-case ex
                  (load (file-name-sans-extension file))
                ('error (with-current-buffer "*scratch*"
                          (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex)))))
           (load (file-name-sans-extension file))))

  (byte-compile-file (concat user-emacs-directory "init.el"))

(provide 'init.el) ;;; init.el ends here
