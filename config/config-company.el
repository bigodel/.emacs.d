;;; config-company.el --- Company configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; install `company-mode' and activate it after init
(require-package 'company)

;; complete unicode mathematical symbols with `company-mode'
(require-package 'company-math)

;; variables
(setvar company-tooltip-align-annotations t) ; align annotations to the right
(setvar company-tooltip-limit 12)        ; limit of completions per pop up
(setvar company-idle-delay nil)          ; only complete when I want to
(setvar company-echo-delay 0)            ; disable blinking
(setvar company-minimum-prefix-length 2) ; minimum chars to start completion
(setvar company-require-match nil)       ; can quit company without completing
(setvar company-selection-wrap-around t) ; wrap when no more candidates
(setvar company-backends                 ; default company backends
        '(;; main completion
          (company-files
           company-keywords
           company-capf)
          ;; plain text
          (company-abbrev
           company-dabbrev
           company-ispell)
          ;; code
          (company-dabbrev-code
           company-semantic
           company-gtags)
          ;; programming languages
          (company-eclim
           company-clang
           company-elisp
           company-css)
          ;; build tools
          (company-xcode
           company-cmake)
          company-bbdb))
(setvar company-global-modes             ; ignored modes
        '(not eshell-mode comint-mode))

;; start company
(global-company-mode)

(defun /company/add-backends (backends &optional is-local)
  "Add BACKENDS to `company-backends'. If `yasnippet' is
installed, append \":with company-yasnippet\" to BACKENDS.

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
          (setvar company-backends
                  (append
                   (list (append backends '(:with company-yasnippet)))
                   company-backends) 'local)
        (setvar company-backends
                (append
                 (list (append backends '(:with company-yasnippet)))
                 company-backends)))
    (if is-local
        (setvar company-backends (append backends company-backends) 'local)
      (setvar company-backends (append backends company-backends)))))

;; add yasnippet as a backend for all backends
(after 'yasnippet
  (setvar company-backends
          (mapcar
           (lambda (backend)
             (if (and (listp backend) (member 'company-yasnippet backend))
                 backend
               (append (if (consp backend) backend (list backend))
                       '(:with company-yasnippet))))
           company-backends)))

;; `company' bindings
(/bindings/define-keys company-active-map
  ((kbd "C-n") #'company-complete-common-or-cycle)
  ((kbd "C-p") #'company-select-previous)
  ((kbd "TAB") #'company-complete-common-or-cycle)
  ((kbd "<backtab>") #'company-select-previous)
  ((kbd "RET") #'company-complete-selection))

;; make `company' work like vim's autocompletion
(after 'evil
  (/bindings/define-keys evil-insert-state-map
    ((kbd "<C-tab>") #'company-yasnippet)
    ((kbd "C-n") #'company-complete)
    ((kbd "C-p") #'company-complete)))

(provide 'config-company)
;;; config-company ends here
