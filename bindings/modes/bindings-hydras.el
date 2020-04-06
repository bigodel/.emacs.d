;;; bindings-hydras.el --- Hydras definitions

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling. You can find all of Bailey Lings' Emacs
;; configuration in https://github.com/bling/dotemacs.
;;
;;; Code:
;;; install hydra
(setvar 'lv-use-separator t)
(require-package 'hydra)
(autoload 'hydra-default-pre "hydra")

(after 'flycheck
  (defhydra hydras/errors (:hint nil)
    "
   errors:  navigation                 flycheck
            -----------------------    ---------------
            _j_ → next error             _l_ → list errors
            _k_ → previous error         _?_ → describe checker
"
    ("j" (call-interactively #'flycheck-next-error))
    ("k" (call-interactively #'flycheck-previous-error))
    ("?" flycheck-describe-checker)
    ("l" flycheck-list-errors :exit t)
    ("q" nil "quit" :exit t)))

(after [counsel projectile counsel-projectile]
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
    ("l" swiper)
    ("L" swiper-all)))

(after 'config-utils
  (defhydra hydras/files/convert (:hint nil :exit t)
    "
   convert to _d_ → dos
              _u_ → unix
"
    ("d" utils-set-buffer-to-dos-format)
    ("u" utils-set-buffer-to-unix-format))

  (defhydra hydras/files (:hint nil :exit t)
    "
   files:  _f_ → find files    → delete  _y_ → copy filename  _E_ → edit as root
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
    ("C" hydras/files/convert/body)))

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
(after 'config-utils
  (defhydra hydras/utils (:hint nil :exit t)
    "
   utils:
"
    ("t" utils-goto-scratch-buffer))

  (bindings-define-prefix-keys bindings-space-map "SPC"
    ("U" #'hydras/utils/body)))

;;; bindings
;; SPC bindings
(bindings-define-prefix-keys bindings-space-map "SPC"
  ("t" #'hydras/toggles/body "toggle...")
  ("j" #'hydras/jumps/body "jump...")
  ("s" #'hydras/search/body "search...")
  ("F" #'hydras/files/body "files...")
  ("i" #'hydras/ivy/body "ivy...")
  ("g" #'hydras/magit/body "magit..."))

;; global bindings
(bindings-define-keys (current-global-map)
  ((kbd "C-x n") #'hydras/narrow/body))

;; space bidings
(bindings-define-prefix-keys bindings-space-map "SPC"
  ("t" #'hydras/toggles/body "toggle...")
  ("j" #'hydras/jumps/body "jump...")
  ("s" #'hydras/search/body "search...")
  ("F" #'hydras/files/body "files...")
  ("i" #'hydras/ivy/body "ivy...")
  ("g" #'hydras/magit/body "magit...")
  ("u" #'hydras/utils/body "utils..."))

(provide 'bindings-hydras)
;;; bindings-hydras.el ends here
