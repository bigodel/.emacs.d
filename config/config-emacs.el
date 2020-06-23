;;; config-emacs.el --- Emacs configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; user settings
(setvar 'user-full-name "João Pedro de Amorim Paula")
(setvar 'user-mail-address "jpedrodeamorim@gmail.com")

;;; server
(setvar 'server-auth-dir (expand-file-name "server" dotemacs-cache-directory))
(require 'server)
(unless (server-running-p)
  (server-start))

;;; emacs window title
(setvar 'frame-title-format
        '(:eval
          (format "%s@%s: %s %s"
                  (or (file-remote-p default-directory 'user)
                      user-real-login-name)
                  (or (file-remote-p default-directory 'host)
                      system-name)
                  (buffer-name)
                  (cond
                   (buffer-file-truename
                    (concat "(" buffer-file-truename ")"))
                   (dired-directory
                    (concat "{" dired-directory "}"))
                   (t
                    "[no file]")))))


;;; move cursor to the last position upon open
(setvar 'save-place-file (expand-file-name "places" dotemacs-cache-directory))
(save-place-mode t)

;;; savehist
(setvar 'savehist-file (expand-file-name "savehist" dotemacs-cache-directory))
(setvar 'savehist-additional-variables '(search
                                         ring
                                         regexp-search-ring
                                         kill-ring))
(setvar 'history-length 1000)
(savehist-mode t)

;;; recent files
(setvar 'recentf-save-file (expand-file-name "recentf"
                                             dotemacs-cache-directory))
(setvar 'recentf-max-saved-items 1000)
(setvar 'recentf-max-menu-items 500)
(setvar 'recentf-auto-cleanup 300)
(setvar 'recentf-exclude dotemacs-globally-ignored-directories)
(recentf-mode t)

(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(add-to-list 'recentf-exclude ".*elpa.*autoloads\.el$")

(run-with-idle-timer 600 t #'recentf-save-list)

;;; save desktop settings and configuration between sessions
(when (display-graphic-p)
  (setvar 'desktop-restore-frames nil)  ; save and restore frame and win config
  (setvar 'desktop-path `(,dotemacs-cache-directory ; paths to look for desktops
                          ,user-emacs-directory
                          ,(getenv "HOME")))
  (setvar 'desktop-dirname (car desktop-path))
  (setvar 'desktop-base-file-name "emacs.desktop") ; default name of the file
  (setvar 'desktop-base-lock-name       ; the default name of the lock file
          (concat desktop-base-file-name ".lock"))
  (setvar 'desktop-save 'if-exists)     ; whether to save desktop when killed
  (desktop-save-mode t))

;;; garbage collector
;; TODO: investigate gcmh (garbage collector magic hack) package and maybe
;; incorporate what it does, without using the package, here, instead of all of
;; this configuration
;;
;; the default value of the garbage collector is too small for packages like
;; lsp, and despite the recommendations of bailey ling, the dude i've basically
;; copied the building blocks for my config from, i'll set it to 16MB, based on
;; doom-emacs, because i have spare memory on the computers i've been using
;;
;; to read more on the gc-cons-threshold, go to:
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(defconst original-gc-cons-threshold gc-cons-threshold-before-init
  "The original value for the `gc-cons-threshold' variable.")

(defvar default-gc-cons-threshold 100000000 ; 16777216
  "The default value for the `gc-cons-threshold' variable.

Note that this is different from its original value.")

;; tell emacs to garbage collect when idle for 5 seconds
;; (run-with-idle-timer 5 t #'garbage-collect)

(defun basic-minibuffer-setup-hook ()
  "Hook to optimize garbage collection when entering or exiting minibuffer.

I'm using `setq' in instead of `setvar' just so I don't have to load a macro."
  (setq gc-cons-threshold most-positive-fixnum))

(defun basic-minibuffer-exit-hook ()
  "Hook to optimize garbage collection when entering or exiting minibuffer.

I'm using `setq' in instead of `setvar' just so I don't have to load a macro.

The `run-at-time' is there so that commands launched immediately
after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold default-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'basic-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'basic-minibuffer-exit-hook)

;;; abbrev
(setvar 'abbrev-mode t)                  ; start `abbrev-mode'
(setvar 'abbrev-file-name                ; default abbrevs file
        (expand-file-name "abbrevs" user-emacs-directory))
(setvar 'save-abbrevs 'silently)         ; save abbrev when file is saved

(defun basic-abbrev-dont-insert-char ()
  "Abbrev hook function, used for `define-abbrev'.
Our use is to prevent inserting the char that triggered
expansion. Put this function as a hook function whenever you
don't want the character that triggered the expansion to be
inserted." t)
;; we need to give the function the no-self-insert propertie
(put 'basic-abbrev-dont-insert-char 'no-self-insert t)

;;; fill column and auto fill
(setvar 'fill-column 80)

(add-hook 'prog-mode-hook
          (lambda ()
            "Only `auto-fill' comments."
            (setvar 'comment-auto-fill-only-comments t 'local)
            (turn-on-auto-fill)))

(add-hook 'text-mode-hook #'turn-on-auto-fill)

;;; ignore case when doing file name completion
(setvar 'pcomplete-ignore-case t)

;;; comint
(setvar 'comint-scroll-to-bottom-on-input 'this) ; scroll to bottom on input
(setvar 'comint-move-point-for-output t) ; output moves point to the end of it

;;; compilation
(setvar 'compilation-always-kill t)      ; always kill compilation process
(setvar 'compilation-ask-about-save nil) ; save without asking

;;; tramp
(setvar 'tramp-persistency-file-name (expand-file-name
                                      "tramp"
                                      dotemacs-cache-directory))

;;; bookmarks
(setvar 'bookmark-default-file (expand-file-name "bookmarks"
                                                 dotemacs-cache-directory))

;;; fringe
(when (display-graphic-p)
  (fringe-mode 8))

;;; ediff
(setvar 'ediff-split-window-function
        'split-window-horizontally)     ; side-by-side diffs
(setvar 'ediff-window-setup-function
        'ediff-setup-windows-plain)     ; no extra frames

;;; ibuffer
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode) ; auto update ibuffer
;; don't ask for confirmation if the buffer isn't modified
(setvar 'ibuffer-expert t)
(setvar 'ibuffer-show-empty-filter-groups nil) ; show empty filter groups
(setvar 'ibuffer-use-other-window t)           ; use other window
;; define filter groups
(setvar 'ibuffer-saved-filter-groups
        '(("home"
           ("erc" (mode . erc-mode))
           ("gnus" (or
                    (mode . message-mode)
                    (mode . bbdb-mode)
                    (mode . mail-mode)
                    (mode . gnus-group-mode)
                    (mode . gnus-summary-mode)
                    (mode . gnus-article-mode)
                    (name . "^\\.bbdb$")
                    (name . "^\\.newsrc-dribble")))
           ("planner" (or           ; add some org mode here later
                       (name . "^\\*Calendar\\*$")
                       (name . "^diary$")))
           ("magit" (or (mode . magit-mode)
                        (name . "^\\*magit.*\\*$")
                        (name . "^magit.*:.*$")))
           ("help" (or
                    (name . "^\\*.*[hH]elp.*\\*$")
                    (name . "^\\*info\\*$")
                    (name . "^\\*Apropos\\*$")))
           ("emacs config" (filename . ".emacs.d"))
           ;; TODO make this work with the var `org-directory'
           ("org" (filename . "docs/org"))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*Load Times\\*$")))
           ("dired" (mode . dired-mode)))))

;; use the above defined filter group
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "home")))

;; use human readable Size column instead of original one
(after 'ibuffer
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

;; modify the default `ibuffer-formats'
(setvar 'ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open Ibuffer with cursor pointed to most recent
buffer name."
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))

;;; move auto-save to cache directory
(let ((dir (expand-file-name "auto-save/" dotemacs-cache-directory)))
  (setvar 'auto-save-list-file-prefix (concat dir "saves-"))
  (setvar 'auto-save-file-name-transforms `((".*" ,(concat dir "save-") t))))

;;; multiple backups
(setvar 'backup-directory-alist
        `((".*" . ,(expand-file-name "backups/" dotemacs-cache-directory))))
(setvar 'backup-by-copying t)
(setvar 'version-control t)
(setvar 'kept-old-versions 0)
(setvar 'kept-new-versions 20)
(setvar 'delete-old-versions t)

;;; scrolling (like in Vim)
(setvar 'scroll-conservatively most-positive-fixnum)
(setvar 'scroll-preserve-screen-position t)
(setvar 'scroll-margin 0)

;;; better buffer names for duplicates
(setvar 'uniquify-buffer-name-style 'forward)
(setvar 'uniquify-separator "/")
(setvar 'uniquify-ignore-buffers-re "^\\*") ; leave special buffers alone
(setvar 'uniquify-after-kill-buffer-p t)

;;; show-paren
(setvar 'show-paren-style 'parenthesis)
(setvar 'show-paren-delay 0)
(setvar 'show-paren-highlight-openparen t)
(setvar 'show-paren-when-point-inside-paren t)
(setvar 'show-paren-when-point-in-periphery nil)
(show-paren-mode t)

;;; don't kill important buffers
(defun basic-dont-kill-important-buffers ()
  "Don't kill a buffer is its *scratch* or *Messages* or *Load Times*."
  (if (member (buffer-name (current-buffer))
              '("*scratch*" "*Messages*"))
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions #'basic-dont-kill-important-buffers)

;;; ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; coding system
(let ((coding 'utf-8))
  (setvar 'locale-coding-system coding)
  (set-selection-coding-system coding)
  (set-default-coding-systems coding)
  (prefer-coding-system coding)
  (setvar 'buffer-file-coding-system coding))

;;; default tabs configuration
(setvar 'indent-tabs-mode nil) ; spaces instead of tabs
(setvar 'tab-width 4)

;;; give me a clean *scratch* buffer upon startup
(setvar 'inhibit-startup-screen t)
(setvar 'inhibit-startup-echo-area-message t)
(setvar 'initial-scratch-message nil)

;;; profiler
(after 'profiler
  ;; by default the width of the columns in the profiler window is too small
  ;; sometimes, although it works on most of the time, so i've added this minor
  ;; mode as shown here: https://kisaragi-hiu.com/blog/2019-05-16-emacs-profiler-width.html
  ;; and here: https://emacs.stackexchange.com/questions/7344/make-profiler-report-columns-wider
  (define-minor-mode profiler-wide-mode
    "Minor mode to widen profiler reports."
    :global t
    (if profiler-wide-mode
        (setf (caar profiler-report-cpu-line-format) 80
              (caar profiler-report-memory-line-format) 80)
      (setf (caar profiler-report-cpu-line-format) 50
            (caar profiler-report-memory-line-format) 55))))

;;; term
;; TODO configurations to make *term better
(setvar 'explicit-shell-file-name (or (getenv "SHELL")
                                      (if (string= system-type "berkley-unix")
                                          "/usr/local/bin/zsh"
                                        "/usr/bin/zsh")))
(defun ansi-term-new-window ()
  "Opens up a new `ansi-term' in the directory of the current file.
The buffer is renamed to match that directory to make multiple
terminal windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (floor (/ (window-total-height) 2.4)))
         ;; (width (/ (window-total-height) 2))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    ;; (split-window-horizontally)
    (other-window 1)
    (ansi-term (or (getenv "SHELL")
                   (executable-find "zsh")
                   (executable-find "bash")) (concat "ansi-term: " name))))

;;; eww
;; set eww as the default browser
;; (setvar 'browse-url-browser-function 'eww-browse-url)

;;; file and dir local variables
(after 'helpful                          ; for some reason helpful resets this
  (setvar 'enable-local-variables :all)) ; always enable local variables
(setvar 'enable-local-eval t)           ; enable eval in local variables
(setvar 'enable-dir-local-variables t)  ; enable dir local variables
(setvar 'enable-remote-dir-locals t)    ; dir-locals applied to remote files

;;; misc
;; TODO: some of this might deserve its own section
;; add a new place to store authentication info
(add-to-list 'auth-sources (expand-file-name ".authinfo"
                                             dotemacs-cache-directory))
(setvar 'confirm-kill-emacs 'y-or-n-p)  ; y-or-n confirmation when quitting
(setvar 'x-gtk-use-system-tooltips nil) ; use emacs tooltips, not gtk's
(setvar 'debug-on-error nil)            ; enter debugger if error is signaled
(setvar 'sentence-end-double-space nil)  ; setences don't end with double space
(setvar 'ring-bell-function 'ignore)     ; disable annoying bell
(setvar 'mark-ring-max 64)               ; max number of marks
(setvar 'global-mark-ring-max 128)       ; max number of global marks
(setvar 'select-enable-clipboard t)      ; use clipboard for cutting and pasting
(setvar 'save-interprogram-paste-before-kill t) ; save clipboard into kill-ring
(setvar 'track-eol t)                   ; vertical motion at eol keeps at eol
(setvar 'create-lockfiles t)            ; create lockfiles (see manual for info)
(setvar 'enable-recursive-minibuffers t) ; recursive minibuffers (be careful)
(setvar 'enable-local-variables nil)     ; don't query about safe variables
(setvar 'mouse-yank-at-point t)          ; don't move point to mouse paste
(setvar 'truncate-lines nil)             ; display or not continuous lines
(setvar 'truncate-partial-width-windows nil) ; respect the value of the above
(toggle-truncate-lines -1)               ; don't truncate!!!!
(setvar 'global-auto-revert-non-file-buffers t) ; revert nonfile buffers (dired)
(global-auto-revert-mode t)             ; revert buffers when files change
(xterm-mouse-mode t)                    ; mouse on in xterm compatible terminals
(electric-indent-mode t)                ; indent automatically on some keys

(add-hook 'text-mode-hook (lambda ()
                            "Set `word-warp' to `t' in `text-mode'."
                            (setvar 'word-wrap t 'local))) ; wrap words

;; create e new notepad file if it doesn't exist
(defconst notepad-file
  (if (not (string= "" (getenv "NOTEPAD")))
      (getenv "NOTEPAD")
    (make-temp-file (concat "notepad-" user-login-name ".") nil nil
                    "# file to take quick notes"))
  "File used to take quick notes on my system.")
;; set it to be in org mode
(add-to-list 'auto-mode-alist `(,notepad-file . org-mode))

(provide 'config-emacs)
;;; config-emacs.el ends here
