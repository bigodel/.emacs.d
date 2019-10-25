;;; config-basic.el --- Basic configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; user settings
(setvar user-full-name "João Pedro de Amorim Paula")
(setvar user-mail-address "jpedrodeamorim@gmail.com")

;;; server
(require 'server)
(defvar dotemacs-basic/server-directory
  (format "%s/emacs%d/" (or (getenv "TMPDIR") "/tmp") (user-uid))
  "The storage location for the socket file used to connect to the daemon.")
(setvar server-socket-dir               ; dir for the server socket
        dotemacs-basic/server-directory)
(setvar server-auth-dir                 ; dir for the authentication files
        (expand-file-name "server" dotemacs-basic/server-directory))
;; start server if its not already running
(unless (server-running-p)
  (server-start))

;;; move cursor to the last position upon open
(setvar save-place-file (expand-file-name "places" dotemacs-cache-directory))
(save-place-mode t)

;;; savehist (minibuffer history)
(setvar savehist-file (expand-file-name "savehist" dotemacs-cache-directory))
(setvar savehist-additional-variables '(search ring regexp-search-ring))
(setvar savehist-autosave-interval 60)
(setvar history-length 100)
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

;; garbage collector
(defun /basic/minibuffer-setup-hook ()
  "Hook to optimize garbage collection when entering or exiting minibuffer."
  (setvar gc-cons-threshold most-positive-fixnum))

(defun /basic/minibuffer-exit-hook ()
  "Hook to optimize garbage collection when entering or exiting minibuffer."
  (setvar gc-cons-threshold (* 64 1024 1024)))

(add-hook 'minibuffer-setup-hook #'/basic/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'/basic/minibuffer-exit-hook)

;;; hippie expand and abbrevs
(setvar abbrev-mode t)                  ; start `abbrev-mode'
(setvar abbrev-file-name                ; default abbrevs file
        (concat user-emacs-directory "abbrevs"))
(setvar save-abbrevs 'silently)         ; save abbrev when file is saved

(/bindings/define-key (current-global-map)
  (kbd "M-/") #'hippie-expand)      ; `hippie-expand' instead of `abbrev-expand'

(defun /basic/abbrev-dont-insert-char ()
  "Abbrev hook function, used for `define-abbrev'.
Our use is to prevent inserting the char that triggered
expansion. Put this function as a hook function whenever you
don't want the character that triggered the expansion to be
inserted." t)
;; we need to give the function the no-self-insert propertie
(put '/basic/abbrev-dont-insert-char 'no-self-insert t)

;;; fill column and auto fill
(setvar fill-column 80)

(defun /basic/comment-auto-fill ()
  "Only `auto-fill' comments."
  (setvar comment-auto-fill-only-comments t 'local)
  (turn-on-auto-fill))

(add-hook 'prog-mode-hook #'/basic/comment-auto-fill)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; ignore case when doing file name completion
(setvar pcomplete-ignore-case t)

;;; comint
(setvar comint-scroll-to-bottom-on-input 'this)

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
    (setvar ls-lisp-use-insert-directory-program nil)
    (require 'ls-lisp)))

;;; tramp
(setvar tramp-persistency-file-name
        (expand-file-name "tramp" dotemacs-cache-directory))
(setvar tramp-default-method "ssh")
(setvar remote-file-name-inhibit-cache nil)
(setvar vc-ignore-dir-regexp (format "%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
;;; bookmarks
(setvar bookmark-default-file
        (expand-file-name "bookmarks" dotemacs-cache-directory))
(setvar bookmark-save-flag 1) ;; save after every change

;;; fringe
(when (display-graphic-p)
  (fringe-mode '(8 . 8)))

;;; ediff
(setvar ediff-split-window-function
        'split-window-horizontally) ; side-by-side diffs
(setvar ediff-window-setup-function
        'ediff-setup-windows-plain) ; no extra frames

;;; ibuffer
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
;; don't ask for confirmation if the buffer isn't modified and some other things
(setvar ibuffer-expert t)
(setvar ibuffer-show-empty-filter-groups nil)
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
           "Open ibuffer with cursor pointed to most recent buffer name."
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))

(/bindings/define-key (current-global-map) (kbd "C-x C-b") #'ibuffer)

;;; move auto-save to the cache
(let ((dir (expand-file-name "auto-save/" dotemacs-cache-directory)))
  (setvar auto-save-list-file-prefix (concat dir "saves-"))
  (setvar auto-save-file-name-transforms `((".*" ,(concat dir "save-") t))))

;;; multiple-backups
(setvar backup-directory-alist
        `((".*" . ,(expand-file-name "backups/" dotemacs-cache-directory))))
(setvar backup-by-copying t)
(setvar version-control t)
(setvar kept-old-versions 2)
(setvar kept-new-versions 20)
(setvar delete-old-versions t)

;;; scrolling
(setvar scroll-conservatively 0)
(setvar scroll-preserve-screen-position t)
(setvar scroll-margin 1)

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
(defun /basic/dont-kill-important-buffers ()
  "Don't kill a buffer is its *scratch* or *Messages* or *Require Times*."
  (if (member (buffer-name (current-buffer))
              '("*scratch*" "*Messages*" "*Require Times*"))
      (progn (bury-buffer) nil)
    t))
(add-hook 'kill-buffer-query-functions #'/basic/dont-kill-important-buffers)

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
;; enable tabs instead of spaces in makefiles
;; TODO: move this to lang-makefile.el maybe???
(add-hook 'makefile-mode-hook '(setvar indent-tabs-mode t 'local))

;;; give me a clean *scratch* buffer upon startup
(setvar inhibit-startup-screen t)
(setvar inhibit-startup-echo-area-message t)
(setvar initial-scratch-message nil)

;;; infer indentation
(defun /basic/infer-indentation-style ()
  "If the file has more tabs than spaces, use tabs instead for indentation.
If it has more spaces, use spaces instead of tabs."
  (interactive)
  (let ((space-count (how-many "^    " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (when (> tab-count space-count)
      (setvar indent-tabs-mode t 'local))))

(defun /basic/find-file-hook ()
  "Run `/basic/infer-indentation-style' after `find-file'.
Also, if the file has '.min' in it, switch to `fundamental-mode'."
  (/basic/infer-indentation-style)
  (when (string-match "\\.min\\." (buffer-file-name))
    (fundamental-mode)))
(add-hook 'find-file-hook #'/basic/find-file-hook)

;;; misc
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
(xterm-mouse-mode t)                    ; mouse on in xterm compatible terminals
(global-auto-revert-mode t)             ; revert buffer when file changes
(electric-indent-mode t)                ; indent automatically on some keys
(delete-selection-mode t)               ; delete region and replace with text
(random t)                              ; random number seed

;;; bindings
;; C-c bindings
(/bindings/define-keys (current-global-map)
  ((kbd "C-c s") #'/util/goto-scratch-buffer "go to scratch")
  ((kbd "C-c e") #'/util/eval-and-replace "eval and replace")
  ((kbd "C-c C-l") #'/util/reload-init-file))

;; C-x bindings
(/bindings/define-keys (current-global-map)
  ((kbd "C-x C") #'compile)
  ((kbd "C-x c") #'recompile)
  ((kbd "C-x C-k") #'kill-this-buffer)
  ((kbd "C-x K") #'/util/delete-current-buffer-file)
  ((kbd "C-x C-S-f") #'/util/find-file-as-root))

;; misc bindings
(/bindings/define-keys (current-global-map)
  ;; since I use a US international keyboard, ' is actually translated as
  ;; <dead-acute> (it is a dead key) so I need to make Emacs understand
  ;; <C-dead-acute> as C-'
  ((kbd "<C-dead-acute>") (kbd "C-'"))
  ((kbd "<M-next>") #'scroll-other-window)
  ((kbd "<M-prior>") #'scroll-other-window-down))

;; escape minibuffer with ESC
;; (/bindings/define-key minibuffer-local-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (/bindings/define-key minibuffer-local-ns-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (/bindings/define-key minibuffer-local-completion-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (/bindings/define-key minibuffer-local-must-match-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (/bindings/define-key minibuffer-local-isearch-map
;;   [escape] #'/util/minibuffer-keyboard-quit)

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (/bindings/define-keys (current-global-map)
    ([mouse-4] (bind (scroll-down 1)))
    ([mouse-5] (bind (scroll-up 1)))))

(provide 'config-basic)
;;; config-basic.el ends here
