;;; config-company.el --- Company configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; install `company-mode' and activate it after init
(require-package 'company)

;; complete unicode mathematical symbols with `company-mode'
(require-package 'company-math)

;; show a help pop up
(require-package 'company-quickhelp)
(setvar 'company-quickhelp-delay 0.5) ; how much secs to wait before showing
(setvar 'company-quickhelp-use-propertized-text nil) ; beautiful text please
(company-quickhelp-mode t)

;;; variables
(setvar 'company-tooltip-align-annotations t) ; align annotations to the right
(setvar 'company-tooltip-limit 12)            ; limit of completions per pop up
(setvar 'company-idle-delay 0.5)        ; idle time to show completion. if set
                                        ; to nil then only complete when asked
(setvar 'company-show-numbers t)         ; show numbers for quick nav with M-num
(setvar 'company-echo-delay (if (display-graphic-p) nil 0)) ; disable blinking
(setvar 'company-minimum-prefix-length 1) ; minimum chars to start completion
(setvar 'company-require-match nil)       ; can quit company without completing
(setvar 'company-selection-wrap-around t) ; wrap when no more candidates
(setvar 'company-dabbrev-other-buffers nil) ; only words of the current buffer
(setvar 'company-backends                 ; default company backends
        '(company-eclim                  ; completion for eclim
          company-semantic               ; CEDET semantic
          company-clang                  ; clang
          company-xcode                  ; Xcode
          company-cmake                 ; CMake
          company-capf                  ; Emacs' completion-at-point-functions
          company-files                 ; files (absolute and relative)
          (company-dabbrev-code
           company-gtags
           company-etags
           company-keywords)
          company-abbrev
          company-dabbrev))

(setvar 'company-global-modes             ; ignored modes
        '(not eshell-mode
              shell-mode
              help-mode
              message-mode
              comint-mode))

;;; start company
(add-hook 'after-init-hook #'global-company-mode)

;; helper functions
(defun company-backend-with-yasnippet (backend)
  "Set up the yasnippet backend with BACKEND."
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun company-add-backends (backends &optional is-local)
  "Add BACKENDS to `company-backends' at the top of the list.
If `yasnippet' is installed, append \":with company-yasnippet\"
each of the to BACKENDS.

If IS-LOCAL is non-nil, only change the value of
`company-backends' on this buffer, otherwise change it globally.

This function depends no the macro `setvar' defined in my Emacs
configuration available at `https://github.com/jpprime/.emacs.d'."
  ;; error if one of the arguments passed as backends is not even a function
  (dolist (backend backends)
    (unless (functionp backend)
      (error "%s is not a function, so it can't be a company backend" backend)))

  ;; check if company is installed
  (if (package-installed-p 'yasnippet)
      ;; if we want it to be a local change
      (if is-local
          (setvar 'company-backends
                  (mapcar #'company-backend-with-yasnippet
                          (append backends company-backends)) 'local)
        (setvar 'company-backends
                (mapcar #'company-backend-with-yasnippet
                        (append backends company-backends))))
    ;; yasnippet not installed
    (if is-local
        (setvar 'company-backends (cons backends company-backends) 'local)
      (setvar 'company-backends (cons backends company-backends)))))

;; add yasnippet as a backend for all backends
(after 'yasnippet
  (setvar 'company-backends
          (mapcar #'company-backend-with-yasnippet company-backends)))

;; if using lsp, install company-lsp. this needs to be down here because we are
;; going to use the functions defined above
(after 'lsp-mode
  (require-package 'company-lsp)        ; install it

  (push 'company-lsp company-backends) ; add it to our backends

  (setvar 'company-lsp-cache-candidates t)     ; better read the docs on this
  (setvar 'company-lsp-async t)          ; fetch completion async
  (setvar 'company-lsp-enable-snippet t) ; enable snippets
  (setvar 'company-lsp-enable-recompletion t)) ; read the docs on this one too

(provide 'config-company)
;;; config-company.el ends here
