
(require 'org)
(setq debug-on-error nil)

(eval-when-compile (require 'cl))

(lexical-let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time
                              (time-subtract (current-time) emacs-start-time))))
                (message "[ Emacs initialized in %.3fs ]" elapsed)))))

(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil)
      (config-directory (concat user-emacs-directory "config/")))

  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  ;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

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

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(load (concat config-directory "init-boot"))

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
