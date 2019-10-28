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
;;; Code:
;;; show possible combinations of keys for prefix keys
(require-package 'which-key)
(setvar which-key-idle-delay 1.0)       ; delay (in secs) for which-key pop up
(setvar which-key-allow-evil-operators t) ; show evil operators with which key
(which-key-mode)

;;; hydra
(setvar lv-use-separator t)
(require-package 'hydra)
(autoload 'hydra-default-pre "hydra")

;; TODO: this package is part of magit, so maybe add this to the vcs
;; configuration file (where magit configuration is)
(require-package 'transient)
(setvar transient-history-file (concat dotemacs-cache-directory
                                       "transient/history.el"))
(setvar transient-levels-file (concat dotemacs-cache-directory
                                      "transient/levels.el"))
(setvar transient-values-file (concat dotemacs-cache-directory
                                      "transient/values.el"))

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

  (binding-define-prefix-keys KEYMAP \"SPC\"
    ((kbd \"SPC\") #'execute-extended-command \"extended command...\"))"
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
This macro uses the `bindings-define-prefix-keys' with the PREFIX arg as nil.
Check its documentation for more details."
  (declare (indent defun))
  `(bindings-define-prefix-keys ,keymap nil ,@body))

(defmacro bindings-define-key (keymap sequence binding &optional description)
  "Define BINDING to be SEQUENCE on KEYMAP. DESCRIPTION is used in `which-key'.
This macro uses the `bindings-define-prefix-keys' macro with the PREFIX arg as
nil. Check its documentation for more details."
  (declare (indent defun))
  `(bindings-define-prefix-keys ,keymap nil
     (,sequence ,binding ,description)))

;;; bindings
;; SPC as a prefix key
(setvar bindings-normal-space-leader-map (make-sparse-keymap))
;; SPC bindings
(bindings-define-prefix-keys bindings-normal-space-leader-map "SPC"
  (" " #'execute-extended-command "M-x")
  ("f" #'find-file)
  ("b" #'switch-to-buffer)
  ("B" #'ibuffer)
  ;; TODO: put this in undo-tree or maybe in misc
  ("u" #'undo-tree-visualize)
  ("'" #'eshell/new-window "eshell")
  ("v" vc-prefix-map)
  ("4" ctl-x-4-map)
  ("5" ctl-x-5-map))

;; C-x bindings
(bindings-define-keys (current-global-map)
  ((kbd "C-x C-b") #'ibuffer)
  ((kbd "C-x C") #'compile)
  ((kbd "C-x c") #'recompile)
  ((kbd "C-x C-k") #'kill-this-buffer)
  ((kbd "C-x K") #'/util/delete-current-buffer-file)
  ((kbd "C-x C-S-f") #'/util/find-file-as-root))

;; C-c bindings
(bindings-define-keys (current-global-map)
  ((kbd "C-c s") #'/util/goto-scratch-buffer "go to scratch")
  ((kbd "C-c e") #'/util/eval-and-replace "eval and replace")
  ((kbd "C-c C-l") #'/util/reload-init-file))

;; misc bindings
(bindings-define-keys (current-global-map)
  ((kbd "M-/") #'hippie-expand)
  ;; since I use a US international keyboard, ' is actually translated as
  ;; <dead-acute> (it is a dead key) so I need to make Emacs understand
  ;; <C-dead-acute> as C-'
  ((kbd "<C-dead-acute>") (kbd "C-'"))
  ((kbd "<M-next>") #'scroll-other-window)
  ((kbd "<M-prior>") #'scroll-other-window-down))

;; eshell
(after 'eshell
  (bindings-define-keys (current-global-map)
    ((kbd "M-!") #'eshell-command)
    ((kbd "C-!") #'eshell/new-window)
    ((kbd "C-c t") #'eshell/new-window)))

;; escape minibuffer with ESC
;; (bindings-define-key minibuffer-local-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (bindings-define-key minibuffer-local-ns-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (bindings-define-key minibuffer-local-completion-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (bindings-define-key minibuffer-local-must-match-map
;;   [escape] #'/util/minibuffer-keyboard-quit)
;; (bindings-define-key minibuffer-local-isearch-map
;;   [escape] #'/util/minibuffer-keyboard-quit)

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (bindings-define-keys (current-global-map)
    ([mouse-4] (bind (scroll-down 1)))
    ([mouse-5] (bind (scroll-up 1)))))

(provide 'bindings)
;; bindings.el ends here
