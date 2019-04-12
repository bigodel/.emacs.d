;;; core-boot.el --- Core file for the boot

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; load custom elisp files in the elisp/ directory
(let ((base (concat user-emacs-directory "elisp/")))
  (when (file-directory-p base)
    (add-to-list 'load-path base)
    (dolist (dir (directory-files base t "^[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; macro to measure the load time of each loaded package
(defmacro /boot/measure-load (target &rest body)
  "Measure load time of each file TARGET with its respective BODY."
  (declare (indent defun))
  `(let ((elapsed)
         (start (current-time)))
     (prog1
         ,@body
       (with-current-buffer (get-buffer-create "*Load Times*")
         (when (= 0 (buffer-size))
           (insert (format "| %-60s | %-23s | elapsed  |\n" "feature" "timestamp"))
           (insert "|------------------------------------------+-------------------------+----------|\n"))
         (goto-char (point-max))
         (setq elapsed (float-time (time-subtract (current-time) start)))
         (insert (format "| %-60s | %s | %f |\n"
                         ,target
                         (format-time-string "%Y-%m-%d %H:%M:%S.%3N"
                                             (current-time))
                         elapsed))))))

(defadvice load (around dotemacs activate)
  "Wrap `/boot/measure-load' around `load'."
  (/boot/measure-load file ad-do-it))

(defadvice require (around dotemacs activate)
  "Wrap `/boot/measure-load' around `require'."
  (if (memq feature features)
      ad-do-it
    (/boot/measure-load feature ad-do-it)))

(defmacro bind (&rest commands)
  "Convenience macro to create lambda interactive COMMANDS."
  `(lambda (arg)
     (interactive "P")
     ,@commands))

(defun require-package (package)
  "Ensure that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "If FILE is loaded execute BODY."
    (declare (indent 1))
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro after (feature &rest body)
  "Load FEATURE then execute BODY.
FEATURE may be any one of:
    'evil              => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]       => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (dolist (f feature)
        (progn
          (setq prog (append `(',f) `(,prog)))
          (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

(defmacro /boot/lazy-major-mode (pattern mode)
  "Define a new `major-mode' matched by PATTERN, install MODE if necessary, and activates it."
  `(add-to-list 'auto-mode-alist
                '(,pattern . (lambda ()
                               (require-package (quote ,mode))
                               (,mode)))))

(defmacro /boot/delayed-init (&rest body)
  "Run BODY after idle for a predetermined amount of time."
  `(run-with-idle-timer 0.5 nil (lambda () ,@body)))

(defmacro setvar (var value &optional comment)
  "Set VAR to VALUE using `customize-set-variable'.
It is also possible to add a COMMENT."
  `(customize-set-variable ',var ,value ,comment))


(provide 'core-boot)
;;; core-boot.el ends here
