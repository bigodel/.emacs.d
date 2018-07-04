;; default location of emacs' files
(setq user-emacs-directory "~/.emacs.d/")

(defun tangle-init ()
  (interactive)
  "If the current buffer is init.org the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

;; execute the tangle function every time init.org is saved
(add-hook 'after-save-hook 'tangle-init)

(setq user-full-name "João Pedro de Amorim Paula"
      user-mail-address "jpedrodeamorim@gmail.com")

(defvar server-socket-dir
  (and (featurep 'make-network-process '(:family local))
       (format "%s/emacs%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))
  "The directory in which to place the server socket. If local
  sockets are not supported, this is nil.")

(setq tls-checktrust t)

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name 'forward)

(require 'saveplace)
(setq-default save-place t)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                          try-expand-dabbrev-all-buffers
                                          try-expand-dabbrev-from-kill
                                          try-complete-file-name-partially
                                          try-complete-file-name
                                          try-expand-all-abbrevs
                                          try-expand-list
                                          try-expand-line
                                          try-complete-lisp-symbol-partially
                                          try-complete-lisp-symbol))

(global-set-key (kbd "M-/") 'hippie-expand)

(require 'package)
(package-initialize)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(add-to-list 'load-path "~/.emacs.d/elisp")
