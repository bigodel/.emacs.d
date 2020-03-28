;;; core-boot.el --- Core file for the boot

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling, I only added a few more stuff to the
;; docstring and changed the names of some macros. You can find all of his Emacs
;; configuration in https://github.com/bling/dotemacs.
;;
;;; Code:
;;; load custom elisp files in the elisp/ directory
(let ((base (concat user-emacs-directory "elisp/")))
  (when (file-directory-p base)
    (add-to-list 'load-path base)
    (dolist (dir (directory-files base t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; macro to measure the load time of each loaded package
(defmacro measure-load (target &rest body)
  "Measure load time of each file TARGET with its respective BODY."
  (declare (indent defun))
  `(let ((elapsed) (start (current-time)))
     (prog1
         ,@body
       (with-current-buffer (get-buffer-create "*Load Times*")
         (when (= 0 (buffer-size))
           (insert
            (format "| %-60s | %-23s | elapsed  |\n" "feature" "timestamp")))
         (goto-char (point-max))
         (setq elapsed (float-time (time-subtract (current-time) start)))
         (insert (format "| %-60s | %s | %f |\n"
                         ,target
                         (format-time-string "%Y-%m-%d %H:%M:%S.%3N"
                                             (current-time))
                         elapsed))))))

(defadvice load (around dotemacs activate)
  "Wrap `measure-load' around `load'."
  (measure-load file ad-do-it))

(defadvice require (around dotemacs activate)
  "Wrap `measure-load' around `require'."
  (if (memq feature features)
      ad-do-it
    (measure-load feature ad-do-it)))

(defmacro bind (&rest commands)
  "Convenience macro to create lambda interactive COMMANDS."
  `(lambda (arg)
     (interactive "P")
     ,@commands))

(defun require-package (package)
  "Ensure that PACKAGE is installed.
First, use `package-installed-p' to check if PACKAGE was
installed via the Emacs package manager, otherwise, try to
`require' PACKAGE; this ensures that we don't require PACKAGE if
it was installed using the package manager. If both of those
fail, run `package-refresh-contents' and install PACKAGE."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature. See
`eval-after-load' for more details about the different forms of
FILE and their semantics."
    (declare (indent 1))
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro after (feature &rest body)
  "Executes BODY after FEATURE has been loaded.
FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

(defmacro setvar (var value &optional local comment)
  "Set VAR to VALUE using `customize-set-variable'.
It is also possible to add a COMMENT, although it is not possible
to add a COMMENT when LOCAL is non-nil. This macro is just a
wrapper around `custmize-set-variable' which sets VAR to VALUE
using `custom-set' if available and `set-default' otherwise. When
LOCAL is not nil, use `setq-local' to change the value of the
variable locally."
  (if local
      `(setq-local ,(eval var) ,value)
    `(customize-set-variable ,var ,value ,comment)))

(defmacro lazy-major-mode (pattern mode)
  "Define a new `major-mode' matched by PATTERN, install MODE if
necessary, and activates it."
  `(add-to-list 'auto-mode-alist
                '(,pattern . (lambda ()
                               (require-package (quote ,mode))
                               (,mode)))))

(defmacro delayed-init (&rest body)
  "Run BODY after idle for a predetermined amount of time."
  `(run-with-idle-timer 0.5 nil (lambda () ,@body)))

(defun create-non-existent-directory (&optional dir)
  "When trying to access non-existing directories, ask to create them.
If DIR is provided, ask to create DIR."
  (let ((parent-directory (or (bound-and-true-p dir)
                              (file-name-directory buffer-file-name))))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory '%s' does not exist! Create it?"
                                 parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

(provide 'core-boot)
;;; core-boot.el ends here
