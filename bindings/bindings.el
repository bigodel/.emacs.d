;;; bindings.el --- General bindings definitions

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling. I made some small changes to the main
;; macro `bindings-define-prefix-keys' that removes the `kbd' call and allows me
;; to use [remap ...], but also makes it necessary to surround every keyboard
;; sequence on (kbd ...).
;;
;; You can find all of Bailey Lings' Emacs configuration in
;; https://github.com/bling/dotemacs.
;;
;; TODO: move each of the packages configuration to their own file
;;; Code:
;;; macros for defining keys
(defmacro bindings-define-prefix-keys (keymap prefix &rest body)
  "Define a PREFIX key for the KEYMAP.
BODY contains commands using the PREFIX. The commands should be

  (KEY DEF DESCRIPTION)

where KEY is a string or a vector of symbols like in
`define-key' (see its documentation for more information on what
type of argument KEY can be).

DEF is anything that can be a key's
definition like in `define-key' (again, look its documentation
for more information).

DESCRIPTION is a string containing the description of DEF to use
in `which-key' (if installed). The description is set through the
function `which-key-add-key-based-replacements' (do I still need
to tell you to look for the documentation for more information?).
If no DESCRIPTION is given it defaults to the name of COMMAND.

Here is an example of usage:

  (bindings-define-prefix-keys KEYMAP \"SPC\"
    ((kbd \"SPC\") #'execute-extended-command \"extended command...\"))

This macro depends on `cl-macs'."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((seq ,(car binding))
                       (func ,(cadr binding))
                       (desc ,(caddr binding)))
                   (define-key ,keymap seq func)
                   (when (and desc (package-installed-p 'which-key))
                     (which-key-add-key-based-replacements
                       (if ,prefix
                           (concat ,prefix " " (key-description seq))
                         (key-description seq))
                       desc))))))

(defmacro bindings-define-keys (keymap &rest body)
  "Define bindings in BODY on KEYMAP.
This macro uses the `bindings-define-prefix-keys' with the PREFIX
arg as nil. Check its documentation for more details."
  (declare (indent defun))
  `(bindings-define-prefix-keys ,keymap nil ,@body))

(defmacro bindings-define-key (keymap sequence binding &optional description)
  "Define BINDING to be SEQUENCE on KEYMAP. DESCRIPTION is used in `which-key'.
This macro uses the `bindings-define-prefix-keys' macro with the
PREFIX arg as nil. Check its documentation for more details."
  (declare (indent defun))
  `(bindings-define-prefix-keys ,keymap nil
     (,sequence ,binding ,description)))

;;; show possible combinations of keys for prefix keys
(require-package 'which-key)
(setvar 'which-key-idle-delay 1.0)       ; delay (in secs) for which-key pop up
(setvar 'which-key-allow-evil-operators t) ; show evil operators with which key
(which-key-mode)

;; TODO: this package is part of magit, so maybe add this to the vcs
;; configuration file (where magit configuration is)
(require-package 'transient)
(setvar 'transient-history-file (concat dotemacs-cache-directory
                                        "transient/history.el"))
(setvar 'transient-levels-file (concat dotemacs-cache-directory
                                       "transient/levels.el"))
(setvar 'transient-values-file (concat dotemacs-cache-directory
                                       "transient/values.el"))

;;; bindings
;;; SPC as a prefix key
(setvar 'bindings-space-map (make-sparse-keymap))
;; SPC bindings
(bindings-define-prefix-keys bindings-space-map "SPC"
  (" " #'execute-extended-command "M-x")
  ("x" ctl-x-map)
  ((kbd "C-j") #'dired-jump)
  ("d" #'dired)
  ("f" #'find-file)
  ("b" #'switch-to-buffer)
  ("I" #'ibuffer)
  ((kbd "C-b") #'ibuffer)
  ("B" #'buffer-menu)
  ("h" help-map "help")
  ("v" vc-prefix-map "vc-prefix-map")
  ("4" ctl-x-4-map "other window")
  ("5" ctl-x-5-map "other frame"))

;; might not have my eshell config
(after 'config-eshell
  (bindings-define-key bindings-space-map "'" #'eshell/new-window)
  (bindings-define-key ctl-x-map "'" #'eshell/new-window))

;;; C-x bindings
(bindings-define-keys (current-global-map)
  ((kbd "C-x C-j") #'dired-jump)
  ((kbd "C-x C-b") #'ibuffer)
  ((kbd "C-x C") #'compile)
  ((kbd "C-x c") #'recompile)
  ((kbd "C-x C-k") #'kill-this-buffer))

;; might not have my utils installed
(after 'config-utils
  (bindings-define-keys (current-global-map)
    ((kbd "C-x K") #'utils-delete-current-buffer-file)
    ((kbd "C-x C-S-f") #'utils-find-file-as-root)))

;; might not have magit installed
(after "magit-autoloads"
  (bindings-define-keys (current-global-map)
    ((kbd "C-x g") #'magit-status)))

;;; C-c bindings
(after 'config-utils
  (bindings-define-keys (current-global-map)
    ((kbd "C-c s") #'utils-goto-scratch-buffer "go to scratch")
    ((kbd "C-c e") #'utils-eval-and-replace "eval and replace")))

;; misc bindings
(bindings-define-keys (current-global-map)
  ([next] #'next-buffer)
  ([prior] #'previous-buffer)
  ((kbd "M-/") #'hippie-expand)
  ;; since I use a US international keyboard, ' is actually translated as
  ;; <dead-acute> (it is a dead key) so I need to make Emacs understand
  ;; <C-dead-aclute> as C-'
  ((kbd "<C-dead-acute>") (kbd "C-'"))
  ;; TODO: create a scroll-other-window that works with pdf-view and doc-view
  ;; https://github.com/politza/pdf-tools/issues/55
  ((kbd "M-J") (bind (scroll-other-window 15)))
  ((kbd "M-K") (bind (scroll-other-window-down 15)))
  ((kbd "<M-next>") #'scroll-other-window)
  ((kbd "<M-prior>") #'scroll-other-window-down))

;; eshell
(after 'eshell
  (bindings-define-keys (current-global-map)
    ((kbd "M-!") #'eshell-command)
    ((kbd "C-!") (if (fboundp 'eshell/new-window)
                     #'eshell/new-window
                   #'eshell) "eshell")
    ((kbd "C-c t") (if (fboundp 'eshell/new-window)
                       #'eshell/new-window
                     #'eshell) "eshell")))

;; escape minibuffer with ESC
(after 'config-utils
  (bindings-define-key minibuffer-local-map
    [escape] #'utils-minibuffer-keyboard-quit)
  (bindings-define-key minibuffer-local-ns-map
    [escape] #'utils-minibuffer-keyboard-quit)
  (bindings-define-key minibuffer-local-completion-map
    [escape] #'utils-minibuffer-keyboard-quit)
  (bindings-define-key minibuffer-local-must-match-map
    [escape] #'utils-minibuffer-keyboard-quit)
  (bindings-define-key minibuffer-local-isearch-map
    [escape] #'utils-minibuffer-keyboard-quit))

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (bindings-define-keys (current-global-map)
    ([mouse-4] (bind (scroll-down 1)))
    ([mouse-5] (bind (scroll-up 1)))))

(provide 'bindings)
;; bindings.el ends here
