;;; bindings-hydras.el --- Hydras definitions

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling. You can find all of Bailey Lings' Emacs
;; configuration in https://github.com/bling/dotemacs.
;;
;;; Code:
;; TODO: maybe add a quit button
(defhydra hydras/errors (:hint nil)
  "
   errors:  navigation                 flycheck
            -----------------------    ---------------
            _j_ → next error             _l_ → list errors
            _k_ → previous error         _?_ → describe checker
"
  ("j" flycheck-next-error)
  ("k" flycheck-previous-error)
  ("?" flycheck-describe-checker)
  ("l" flycheck-list-errors :exit t))



;; TODO: review this
(defhydra hydras/jumps (:hint nil :exit t)
  "
   jump   _i_ → outline in current buffer   _l_ → lines in current buffer
          _b_ → bookmarks                   _L_ → lines in all buffers
"
  ;; TODO: might want to use counsel-imenu!
  ("i" counsel-imenu)
  ("l" swiper)
  ("L" swiper-all)
  ("b" bookmark-jump))



(defhydra hydras/search (:hint nil :exit t)
  "
    search     directory    ^^project      ^^buffer       ^^buffers
               ---------    ^^-------      ^^------       ^^-------
               _r_ → rg       _R_ → rg       _l_ → lines    _L_ → lines
               _a_ → ag       _A_ → ag
               _p_ → pt       _P_ → pt
               _g_ → grep     _G_ → grep
"
  ("r" counsel-rg)
  ("a" counsel-ag)
  ("p" counsel-pt)
  ("g" counsel-grep)
  ("R" counsel-projectile-rg)
  ("A" counsel-projectile-ag)
  ("P" projectile-pt)
  ("G" counsel-projectile-grep)
  ("l" hydras/jumps/lambda-l-and-exit)
  ("L" hydras/jumps/lambda-L-and-exit))



(defhydra hydras/files/convert (:hint nil :exit t)
  "
   convert to _d_ → dos
              _u_ → unix
"
  ("d" utils-set-buffer-to-dos-format)
  ("u" utils-set-buffer-to-unix-format))

(defhydra hydras/files (:hint nil :exit t)
  "
   files:  _f_ → find files  _D_ → delete  _y_ → copy filename  _E_ → edit as root
           _r_ → recentf     _R_ → rename  _c_ → copy file      _C_ → convert
"
  ;; TODO: ("D" utils-delete-buffer-file)
  ("R" utils-rename-current-buffer-file)
  ("M" utils-rename-buffer-file)
  ("f" counsel-find-file)
  ("r" counsel-recentf)
  ("y" utils-copy-file-name-to-clipboard)
  ("E" utils-find-file-as-root)
  ("c" copy-file)
  ("C" hydras/files/convert/body))



;; TODO: add more things to toggle
(defhydra hydras/toggles (:hint nil :exit t)
  "
   toggle:  _a_ → aggressive indent   _s_ → flycheck   _r_ → read only      _t_ → truncate lines   _e_ → debug on error
            _f_ → auto-fill           _S_ → flyspell   _c_ → completion     _W_ → word wrap        _g_ → debug on quit
            _w_ → whitespace          ^ ^              ^ ^                  _b_ → page break
"
  ("a" aggressive-indent-mode)
  ("c" company-mode)
  ("t" toggle-truncate-lines)
  ("e" toggle-debug-on-error)
  ("g" toggle-debug-on-quit)
  ("b" page-break-lines-mode)
  ("s" flycheck-mode)
  ("S" flyspell-mode)
  ("w" whitespace-mode)
  ("W" toggle-word-wrap)
  ("r" read-only-mode)
  ("f" auto-fill-mode))



(defhydra hydras/ivy (:hint nil :exit t)
  "
   ivy:  _b_ → buffers    _y_ → kill-ring   _l_ → swiper
         _e_ → recentf    _x_ → M-x         _L_ → swiper (multi)
         _f_ → files
"
  ;; TODO: maybe ivy-everything and maybe just ivy-switch-buffer
  ;; ("b" /ivy/everything)
  ("b" ivy-switch-buffer)
  ("e" counsel-recentf)
  ("f" counsel-find-file)
  ("y" counsel-yank-pop)
  ("x" counsel-M-x)
  ("l" swiper)
  ("L" swiper-all))



;; TODO: investigate the need of this
;; (autoload 'magit-blame "magit-blame" nil t)
;; (autoload 'magit-diff "magit-diff" nil t)
;; (autoload 'magit-log "magit-log" nil t)

(defhydra hydras/magit (:hint nil :exit t)
  "
   magit:  _s_ → status  _l_ → log    _f_ → file   _a_ → stage file
           _c_ → commit  _d_ → diff   _z_ → stash  _r_ → unstage file
           _p_ → push    _b_ → blame  _m_ → merge

"
  ("s" magit-status)
  ("b" magit-blame)
  ("f" magit-file-dispatch)
  ("z" magit-stash)
  ("l" magit-log)
  ("d" magit-diff)
  ("c" magit-commit)
  ("m" magit-merge)
  ("p" magit-push)
  ("a" magit-stage-file)
  ("r" magit-unstage-file))



(defhydra hydras/narrow (:hint nil :exit t)
  "
   narrow:  _d_ → defun   _b_ → org-block    _w_ → widen
            _n_ → region  _e_ → org-element
            _p_ → page    _s_ → org-subtree
"
  ("b" org-narrow-to-block)
  ("e" org-narrow-to-element)
  ("s" org-narrow-to-subtree)
  ("d" narrow-to-defun)
  ("n" narrow-to-region)
  ("p" narrow-to-page)
  ("w" widen))



;; TODO: this hydras/utils
(defhydra hydras/utils (:hint nil :exit t)
  "
   utils:
"
  ("t" utils-goto-scratch-buffer))


(provide 'bindings-hydras)
;;; bindings-hydras.el ends here
