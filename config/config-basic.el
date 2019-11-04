;;; config-basic.el --- Basic configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; user settings
(setvar user-full-name "João Pedro de Amorim Paula")
(setvar user-mail-address "jpedrodeamorim@gmail.com")

;;; ask for confirmation before killing emacs
(setvar confirm-kill-emacs 'y-or-n-p)

;;; server
(setvar server-auth-dir (concat dotemacs-cache-directory "server"))
(require 'server)
(unless (server-running-p)
  (server-start))

;;; move cursor to the last position upon open
(setvar save-place-file (concat dotemacs-cache-directory "places"))
(save-place-mode t)

;;; savehist
(setvar savehist-file (concat dotemacs-cache-directory "savehist"))
(setvar savehist-additional-variables '(search ring regexp-search-ring))
(setvar history-length 1000)
(savehist-mode t)

;;; recent files
(setvar recentf-save-file (concat dotemacs-cache-directory "recentf"))
(setvar recentf-max-saved-items 1000)
(setvar recentf-max-menu-items 500)
(setvar recentf-auto-cleanup 300)
(setvar recentf-exclude dotemacs-globally-ignored-directories)
(recentf-mode t)

(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(add-to-list 'recentf-exclude ".*elpa.*autoloads\.el$")

(run-with-idle-timer 600 t #'recentf-save-list)

;;; garbage collector
(defun basic-minibuffer-setup-hook ()
  "Hook to optimize garbage collection when entering or exiting minibuffer."
  (setvar gc-cons-threshold most-positive-fixnum))

(defun basic-minibuffer-exit-hook ()
  "Hook to optimize garbage collection when entering or exiting minibuffer."
  (setvar gc-cons-threshold (* 64 1024 1024)))

(add-hook 'minibuffer-setup-hook #'basic-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'basic-minibuffer-exit-hook)

;;; abbrev
(setvar abbrev-mode t)                  ; start `abbrev-mode'
(setvar abbrev-file-name                ; default abbrevs file
        (concat user-emacs-directory "abbrevs"))
(setvar save-abbrevs 'silently)         ; save abbrev when file is saved

(defun basic-abbrev-dont-insert-char ()
  "Abbrev hook function, used for `define-abbrev'.
Our use is to prevent inserting the char that triggered
expansion. Put this function as a hook function whenever you
don't want the character that triggered the expansion to be
inserted." t)
;; we need to give the function the no-self-insert propertie
(put 'basic-abbrev-dont-insert-char 'no-self-insert t)

;;; fill column and auto fill
(setvar fill-column 80)

(add-hook 'prog-mode-hook
          (lambda ()
            "Only `auto-fill' comments."
            (setvar comment-auto-fill-only-comments t 'local)
            (turn-on-auto-fill)))

(add-hook 'text-mode-hook #'turn-on-auto-fill)

;;; ignore case when doing file name completion
(setvar pcomplete-ignore-case t)

;;; comint
(setvar comint-scroll-to-bottom-on-input 'this) ; scroll to bottom on input
(setvar comint-move-point-for-output t) ; output moves point to the end of it

;;; compilation
(setvar compilation-always-kill t)      ; always kill compilation process
(setvar compilation-ask-about-save nil) ; save without asking

;;; dired
;; wdired
(setvar wdired-allow-to-change-permissions t)
(setvar wdired-allow-to-redirect-links t)
(setvar wdired-use-interactive-rename t)
(setvar wdired-confirm-overwrite t)
;; dired-x is a library to add extra functionality to dired, for more info refer
;; to the GNU manual
;; https://www.gnu.org/software/emacs/manual/html_node/dired-x/
(add-hook 'dired-load-hook
          (lambda ()
            "Load `dired-x' and `dired-aux' when `dired' first loads.
Set `dired-x' and `dired-aux' global variables here."
            (require 'dired-x)
            (require 'dired-aux)
            ;; Set dired-x and dired-aux global variables here. For example:
            ;; (setvar dired-guess-shell-gnutar "gtar")
            ;; (setvar dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            "Activate `hl-line-mode' when `dired' starts. Set
`dired-x' and `dired-aux' buffer-local variables here."
            ;; Set dired-x and dired-aux buffer-local variables here.
            ;; For example:
            ;; (dired-omit-mode 1)
            (hl-line-mode)
            ))

;; *BSD's 'ls' command does not support the "--dired" option needed by Emacs
;; alternatively, we check if 'gls' is installed, which is GNU's ls, and if not
;; we use Emacs's own emulation of 'ls'
(when (string= system-type "berkeley-unix")
  (if (executable-find "gls")
      (setvar insert-directory-program (executable-find "gls"))
    (setvar dired-use-ls-dired nil)
    (setvar ls-lisp-use-insert-directory-program nil)))

;;; tramp
(setvar tramp-persistency-file-name (concat dotemacs-cache-directory "tramp"))

;;; bookmarks
(setvar bookmark-default-file (concat dotemacs-cache-directory "bookmarks"))

;;; fringe
(when (display-graphic-p)
  (fringe-mode 8))

;;; ediff
(setvar ediff-split-window-function
        'split-window-horizontally)     ; side-by-side diffs
(setvar ediff-window-setup-function
        'ediff-setup-windows-plain)     ; no extra frames

;;; ibuffer
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode) ; auto update ibuffer
;; don't ask for confirmation if the buffer isn't modified
(setvar ibuffer-expert t)
(setvar ibuffer-show-empty-filter-groups nil) ; show empty filter groups
(setvar ibuffer-use-other-window t)           ; use other window
;; define filter groups
(setvar ibuffer-saved-filter-groups
        (quote (("Home"
                 ("ERC" (mode . erc-mode))
                 ("Gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))
                 ("Planner" (or           ; add some org mode here later
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")))
                 ("Magit" (or (mode . magit-mode)
                              (name . "^\\*magit.*\\*$")
                              (name . "^magit.*:.*$")))
                 ("Help" (or
                          (name . "^\\*.*[hH]elp.*\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*Apropos\\*$")))
                 ("Emacs config" (filename . ".emacs.d"))
                 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*Load Times\\*$")))
                 ("Dired" (mode . dired-mode))))))

;; use the above defined filter group
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "Home")))

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
(setvar ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent
buffer name."
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))

;;; move auto-save to cache directory
(let ((dir (concat dotemacs-cache-directory "auto-save/")))
  (setvar auto-save-list-file-prefix (concat dir "saves-"))
  (setvar auto-save-file-name-transforms `((".*" ,(concat dir "save-") t))))

;;; multiple backups
(setvar backup-directory-alist
        `((".*" . ,(expand-file-name
                    (concat dotemacs-cache-directory "backups/")))))
(setvar backup-by-copying t)
(setvar version-control t)
(setvar kept-old-versions 0)
(setvar kept-new-versions 20)
(setvar delete-old-versions t)

;;; scrolling (like in Vim)
(setvar scroll-conservatively most-positive-fixnum)
(setvar scroll-preserve-screen-position t)
(setvar scroll-margin 0)

;;; better buffer names for duplicates
(setvar uniquify-buffer-name-style 'forward)
(setvar uniquify-separator "/")
(setvar uniquify-ignore-buffers-re "^\\*") ; leave special buffers alone
(setvar uniquify-after-kill-buffer-p t)

;;; paren
(setvar show-paren-delay 0)

(after 'paren
  (set-face-background 'show-paren-match "white")
  (set-face-foreground 'show-paren-match "black")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(show-paren-mode t)

;;; don't kill important buffers
(defun basic-dont-kill-important-buffers ()
  "Don't kill a buffer is its *scratch* or *Messages* or *Load Times*."
  (if (member (buffer-name (current-buffer))
              '("*scratch*" "*Messages*" "*Load Times*"))
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions #'basic-dont-kill-important-buffers)

;;; ask y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; coding system
(let ((coding 'utf-8))
  (setvar locale-coding-system coding)
  (set-selection-coding-system coding)
  (set-default-coding-systems coding)
  (prefer-coding-system coding)
  (setvar buffer-file-coding-system coding))

;;; tabs
(setvar indent-tabs-mode nil) ; spaces instead of tabs
(setvar tab-width 4)
;; enable tabs instead of spaces in makefiles
;; TODO: move this to config-makefile.el maybe???
(add-hook 'makefile-mode-hook '(setvar indent-tabs-mode t 'local))

;;; give me a clean *scratch* buffer upon startup
(setvar inhibit-startup-screen t)
(setvar inhibit-startup-echo-area-message t)
(setvar initial-scratch-message nil)

;;; misc variables
(setvar sentence-end-double-space nil)  ; setences don't end with double space
(setvar ring-bell-function 'ignore)     ; disable annoying bell
(setvar mark-ring-max 64)               ; max number of marks
(setvar global-mark-ring-max 128)       ; max number of global marks
(setvar select-enable-clipboard t)      ; use clipboard for cutting and pasting
(setvar save-interprogram-paste-before-kill nil) ; save clipboard into kill-ring
(setvar track-eol t)                    ; vertical motion at eol keeps at eol
(setvar create-lockfiles t)             ; create lockfiles (see manual for info)
(setvar enable-recursive-minibuffers t) ; recursive minibuffers (be careful)
(setvar truncate-lines nil)             ; display or not continuous lines
(setvar mouse-yank-at-point t)          ; don't move point to mouse paste
(global-auto-revert-mode t)             ; revert buffers when files change
(xterm-mouse-mode t)                    ; mouse on in xterm compatible terminals
(electric-indent-mode t)                ; indent automatically on some keys
(random t)                              ; random number seed

(provide 'config-basic)
;;; config-basic.el ends here
