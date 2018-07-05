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

(fset 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq uniquify-buffer-name 'forward)

(save-place-mode t)

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

(setq-default abbrev-mode t)

;; remove the delay
(setq show-paren-delay 0)
(show-paren-mode t)

(setq-default tab-width 4                   ; a tab is 4 spaces
              c-basic-offset 'tab-width     ; default C indentation
              lisp-indent-offset 'tab-width ; default lisp indentation
              indent-tabs-mode nil)         ; spaces instead of tabs

(defun jpprime/infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(add-hook 'prog-mode-hook 'jpprime/infer-indentation-style)

(setq auto-revert-interval 1                   ; refresh buffers fast
      apropos-do-all t                         ; apropos search more extensively
      default-input-method "portuguese-prefix" ; i'm brazilian
      inhibit-startup-message t                ; no splash screen please
      initial-scratch-message nil              ; clean scratch buffer
      recentf-max-saved-items 100              ; show more recent files
      ring-bell-function 'ignore               ; quiet
      visible-bell t                           ; flash the frame to represent bell
      save-interprogram-paste-before-kill t    ; integrate clipboard with kill ring
      require-final-newline t)                 ; always end a file with newline

(setq save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(recentf-mode 1)

(defun jpprime/insert-line-below ()
  "Insert a line below the cursor."
  (interactive)
  (let ((current-point (point)))
    (move-end-of-line 1)
    (open-line 1)
    (goto-char current-point)))

(defun jpprime/insert-line-above ()
  "Insert a line above the cursor."
  (interactive)
  (let ((current-point (point)))
    (move-beginning-of-line 1)
    (newline-and-indent)
    (indent-according-to-mode)
    (goto-char current-point)
    (forward-char)))

(global-set-key (kbd "C-S-n") 'jpprime/insert-line-below)
(global-set-key (kbd "C-S-o") 'jpprime/insert-line-above)

(defun jpprime/toggle-window-split ()
  (interactive)
    (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
            (next-win-buffer (window-buffer (next-window)))
            (this-win-edges (window-edges (selected-window)))
            (next-win-edges (window-edges (next-window)))
            (this-win-2nd
             (not (and (<= (car this-win-edges)
                        (car next-win-edges))
                    (<= (cadr this-win-edges)
                        (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
                 (car (window-edges (next-window))))
              'split-window-horizontally
            'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

;; C-x 4 t jpprime/toggle-window-split
(define-key ctl-x-4-map "t" 'jpprime/toggle-window-split)

(require 'package)

;; do not activate installed packages on startup
(setq package-enable-at-startup nil)

;; configure some some repositorys to fetch packages from
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t))

;; packages installed manually go here
(add-to-list 'load-path "~/.emacs.d/elisp")

;; activate installed packages
(package-initialize)

(require 'tramp)
(setq tramp-default-method "ssh")

;; from the TRAMP FAQ {{{
;; disable version control to avoid delays
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; TODO: move this to the modeline section
;; show a modeline indication when working on root
(defun my-mode-line-function ()
  (when (string-match "^/su\\(do\\)?:" default-directory)
    (setq mode-line-format
          (format-mode-line mode-line-format 'font-lock-warning-face))))
;; }}}

(add-hook 'find-file-hook 'my-mode-line-function)
(add-hook 'dired-mode-hook 'my-mode-line-function)

;; TODO review the necessity of this
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(add-hook 'eshell-mode-hook
          (lambda ()
            (nlinum-mode -1)))

(global-set-key (kbd "M-!") 'eshell-command)
