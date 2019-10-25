;;; core-bindings.el --- Configuration for the bindings

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; macros for defining keys
(defmacro /bindings/define-prefix-keys (keymap prefix &rest body)
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

  (/binding/define-prefix-keys KEYMAP \"SPC\"
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

(defmacro /bindings/define-keys (keymap &rest body)
  "Define bindings in BODY on KEYMAP.
This macro uses the `/bindings/define-prefix-keys' with the PREFIX arg as nil.
Check its documentation for more details."
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil ,@body))

(defmacro /bindings/define-key (keymap sequence binding &optional description)
  "Define BINDING to be SEQUENCE on KEYMAP. DESCRIPTION is used in `which-key'.
This macro uses the `/bindings/define-prefix-keys' macro with the PREFIX arg as
nil. Check its documentation for more details."
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil
     (,sequence ,binding ,description)))

(provide 'core-bindings)
;; core-bindings.el ends here
