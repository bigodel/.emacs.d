
(defvar dotemacs-core/default-coding-system 'utf-8
  "The default coding system to use.")

(defvar dotemacs-core/server-directory
  (format "%s/emacs%d/" (or (getenv "TMPDIR") "/tmp") (user-uid))
  "The storage location for the socket file used to connect to the daemon.")
(setq server-socket-dir dotemacs-core/server-directory)
(setq server-auth-dir (concat dotemacs-core/server-directory "server"))
(require 'server)
(unless (server-running-p)
  (server-start))

(defun /core/create-non-existent-directory ()
  "When trying to access non-exising directories, ask to create them."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
             (y-or-n-p
              (format "Directory `%s' does not exist! Create it?"
                      parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'/core/create-non-existent-directory)

(setq user-full-name "João Pedro de Amorim Paula")
(setq user-mail-address "jpedrodeamorim@gmail.com")

;; move cursor to the last position upon open
(require 'saveplace)
(setq save-place-file (concat dotemacs-cache-directory "places"))
(save-place-mode t)

;; savehist
(setq savehist-file (concat dotemacs-cache-directory "savehist")
      savehist-additional-variables '(search ring regexp-search-ring)
      savehist-autosave-interval 60
      history-length 1000)
(savehist-mode t)

;; recent files
(require 'recentf)
(setq recentf-save-file (concat dotemacs-cache-directory "recentf"))
(setq recentf-max-saved-items 1000)
(setq recentf-max-menu-items 500)
(setq recentf-auto-cleanup 300)
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(add-to-list 'recentf-exclude ".*elpa.*autoloads\.el$")
(recentf-mode t)
(run-with-idle-timer 600 t #'recentf-save-list)

;; gc
(defun /core/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun /core/minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 64 1024 1024)))
(add-hook 'minibuffer-setup-hook #'/core/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'/core/minibuffer-exit-hook)

(require 'whitespace)

;; (setq whitespace-display-mappings
;;       '((space-mark 32 [183])
;;         (newline-mark 10 [182 10])
;;         (tab-mark 9 [9655 9] [92 9])))

(setq whitespace-style '(face trailing tabs lines-tail))

;; (set-face-attribute 'whitespace-space nil
;;                     :background nil
;;                     :foreground "black")

(set-face-attribute 'whitespace-trailing nil
                    :background "gray15")

(global-whitespace-mode t)

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

(setq save-abbrevs 'silently)

(add-hook 'after-save-hook '/util/tangle-init)

;; pcomplete
(setq pcomplete-ignore-case t)

;; imenu
(setq-default imenu-auto-rescan t)

;; narrowing
(put 'narrow-to-region 'disabled nil)

;; dired
(after 'dired
  (require 'dired-x))

;; url
(setq url-configuration-directory (concat dotemacs-cache-directory "url/"))

;; tramp
(setq tramp-persistency-file-name (concat dotemacs-cache-directory "tramp"))
(setq tramp-default-method "ssh")
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; comint
(after 'comint
  (defun /core/toggle-comint-scroll-to-bottom-on-output ()
    (interactive)
    (if comint-scroll-to-bottom-on-output
        (setq comint-scroll-to-bottom-on-output nil)
      (setq comint-scroll-to-bottom-on-output t))))

;; compile
(setq compilation-always-kill t)
(setq compilation-ask-about-save nil)
(add-hook 'compilation-filter-hook
          (lambda ()
            (when (eq major-mode 'compilation-mode)
              (require 'ansi-color)
              (let ((inhibit-read-only t))
                (ansi-color-apply-on-region (point-min) (point-max))))))

;; bookmarks
(setq bookmark-default-file (concat dotemacs-cache-directory "bookmarks"))
(setq bookmark-save-flag 1) ;; save after every change

;; fringe
(when (display-graphic-p)
  (fringe-mode '(8 . 8)))

;; ediff
(setq ediff-split-window-function 'split-window-horizontally) ;; side-by-side diffs
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;; no extra frames

;; re-builder
(setq reb-re-syntax 'string) ;; fix backslash madness

;; clean up old buffers periodically
(midnight-mode)
(midnight-delay-set 'midnight-delay 0)

;; ibuffer
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

;; move auto-save to the cache
(let ((dir (expand-file-name (concat dotemacs-cache-directory "auto-save/"))))
  (setq auto-save-list-file-prefix (concat dir "saves-"))
  (setq auto-save-file-name-transforms `((".*" ,(concat dir "save-") t))))

;; multiple-backups
(setq backup-directory-alist `((".*" . ,(expand-file-name (concat dotemacs-cache-directory "backups/")))))
(setq backup-by-copying t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 20)
(setq delete-old-versions t)

;; better scrolling
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t
      scroll-margin 1)

;; better buffer names for duplicates
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)

(require 'paren)
(set-face-background 'show-paren-match (face-foreground 'default))
(set-face-foreground 'show-paren-match (face-background 'default))
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode 1)

(setq show-paren-delay 0)

(defun /core/do-not-kill-scratch-buffer ()
  (if (member (buffer-name (current-buffer))
              '("*scratch*" "*Messages*" "*Require Times*"))
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions '/core/do-not-kill-scratch-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

(let ((coding 'utf-8))
  (setq locale-coding-system coding)
  (set-selection-coding-system coding)
  (set-default-coding-systems coding)
  (prefer-coding-system coding)
  (setq-default buffer-file-coding-system coding))

(setq sentence-end-double-space nil)
(setq ring-bell-function 'ignore)
(setq mark-ring-max 64)
(setq global-mark-ring-max 128)
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill nil)
(setq create-lockfiles nil)
(setq echo-keystrokes 0.01)
(setq eval-expression-print-level nil)

(setq-default indent-tabs-mode nil) ;; spaces instead of tabs
(setq-default tab-width 4)

(defun /core/infer-indentation-style ()
  "If our source file uses tabs, we use tabs, if spaces spaces,
and if neither, we use the current indent-tabs-mode"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(add-hook 'prog-mode-hook #'/core/infer-indentation-style)

(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode)
(xterm-mouse-mode t)
(which-function-mode t)
(blink-cursor-mode -1)
(global-auto-revert-mode t)
(electric-indent-mode t)
(transient-mark-mode t)
(delete-selection-mode t)
(random t) ;; seed

(defun /core/find-file-hook ()
  (when (string-match "\\.min\\." (buffer-file-name))
    (fundamental-mode)))
(add-hook 'find-file-hook #'/core/find-file-hook)

(provide 'init-core)
