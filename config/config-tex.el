;;; lang-tex.el --- TeX configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(after 'tex-mode
  (require-package 'auctex)

  ;;; variables
  (setvar tex-fontify-script nil)        ; fontify super and subscript
  (setvar font-latex-fontify-script nil) ; fontify super and subscript
  (setvar TeX-master 'dwim)              ; master file for the current buffer
  (setvar TeX-auto-save t)               ; enable parse on save
  (setvar TeX-parse-self t)              ; enable parse on load
  (setvar TeX-save-query nil)            ; don't ask for permission to save
  (setvar TeX-view-program-selection
          '((output-pdf "PDF Tools")
            (output-pdf "Zathura")
            (output-dvi "xdvi")))       ; list of predicates and viewers
  (setvar TeX-show-compilation t)       ; show output of TeX compilation
  (setvar TeX-after-compilation-finished-functions
          '(TeX-revert-document-buffer  ; if using doc-view, revert pdf buffer
            ;; TODO: maybe put this in a defun instead of using lambda
            (lambda (_output)
              "Close compilation buffer if there are no errors.
This function actually checks the values of `TeX-debug-bad-boxes'
and `TeX-debug-warnings' and only closes the buffer if there are
no bad-boxes or warnings, according to the value of the
variables.

Add this function to `TeX-after-compilation-finished-functions'.

Got this from: https://emacs.stackexchange.com/q/38258 and made
just a few changes."
              (let ((buf (TeX-active-buffer)))
                (when (buffer-live-p (TeX-active-buffer))
                  (with-current-buffer (TeX-active-buffer)
                    (when (progn (TeX-parse-all-errors)
                                 (if TeX-debug-bad-boxes
                                     (if TeX-debug-warnings
                                         (null TeX-error-list)
                                       (and
                                        (null (assoc 'bad-box TeX-error-list))
                                        (null (assoc 'error TeX-error-list))))
                                   (if TeX-debug-warnings
                                       (and
                                        (null (assoc 'warning TeX-error-list))
                                        (null (assoc 'error TeX-error-list)))
                                     (null (assoc 'error TeX-error-list)))))
                      (cl-loop for win in (window-list)
                               if (eq (window-buffer win) (current-buffer))
                               do (delete-window win)))))))))
  (setvar TeX-source-correlate-mode t)    ; forward and reverse search
  (setvar TeX-PDF-mode t)                 ; compile to pdf by default
  (setvar TeX-interactive-mode t)         ; pause with error prompt
  (setvar TeX-debug-bad-boxes nil)        ; overfull/underfull box warnings
  (setvar TeX-debug-warnings t)           ; treat warnings as errors
  (setvar LaTeX-math-abbrev-prefix "C-;") ; latex-math prefix, default is `

  ;;; reftex
  ;; start `reftex' and `LaTeX-math-mode' on `LaTeX-mode'
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  ;; `reftex' variables
  (setvar reftex-plug-into-AUCTeX t)      ; integrate reftex with AUCTeX
  (setvar reftex-idle-time 0.5)           ; time Emacs idles before redisplay
  (setvar reftex-auto-recenter-toc t)     ; automatic recenter *TOC* window

  ;; initialize reftex toc mode in emacs state
  (after [evil reftex]
    (evil-set-initial-state 'reftex-toc-mode 'emacs))

  ;; install `ivy-bibtex' if `ivy' is installed
  (after [ivy tex]
    (require-package 'ivy-bibtex)
    (/bindings/define-key tex-mode-map (kbd "C-c c B") #'ivy-bibtex)
    (/bindings/define-key TeX-mode-map (kbd "C-c c B") #'ivy-bibtex))

  ;;; auxiliary modes
  ;; after company is loaded add some company backends for latex to company
  (after 'company
    (add-hook
     'TeX-mode-hook
     (lambda ()
       "Add `company-latex-commands' and
`company-math-symbols-latex' to `company-backends' in
`TeX-mode'."
       (company-add-backends
        '(company-math-symbols-latex company-latex-commands) t))))

  ;;; abbrev-mode
  ;; this is needed so that I can sepcify that on *TeX backslashes are treated
  ;; as part of the abbrev, so I can define abbrevs that start with a backslash.
  ;; The function `/basic/after-abbrev-expand' is a dummy function to not insert
  ;; the character that triggered the abbrev
  (define-abbrev-table 'tex-mode-abbrev-table
    '(("\\c" "\\c{c}"))
    "Custom abbrev-table for `TeX-mode' to allow abbrevs with the
backslash character '\' as part of it."
    :regexp "\\(\\\\[a-z0-9@]+\\)")

  (define-abbrev-table 'plain-tex-mode-abbrev-table
    '(("\\c" "\\c{c}"))
    "Custom abbrev-table for `plain-TeX-mode' to allow abbrevs with the
backslash character '\' as part of it."
    :regexp "\\(\\\\[a-z0-9@]+\\)")

  (define-abbrev-table 'latex-mode-abbrev-table
    '(("\\c" "\\c{c}"))
    "Custom abbrev-table for `LaTeX-mode' to allow abbrevs with the
backslash character '\' as part of it."
    :regexp "\\(\\\\[a-z0-9@]+\\)")

  ;;; TeX mode bindings
  ;; auxiliary functions
  ;;   (defun /tex/TeX-command-run-all-this-window (arg)
  ;;     "`TeX-command-run-all' that compiles and stays on the same window.
  ;; ARG is passed to `TeX-command-run-all' and works the same way.

  ;; This is only effective if using `doc-view-mode' or
  ;; `pdf-view-mode' as the default pdf viewer for Emacs, since when
  ;; it compiles it moves us to the other window."
  ;;     (interactive "P")
  ;;     (save-selected-window
  ;;       (TeX-command-run-all arg)))

  ;; (/bindings/define-key TeX-mode-map
  ;;   [remap TeX-command-run-all] #'/tex/TeX-command-run-all-this-window)

  (defun /tex/LaTeX-insert-item ()
    "Like `LaTeX-insert-item', but always creates a blank newline
instead of breaking where the point is at."
    (interactive "*")
    (let ((env (LaTeX-current-environment)))
      (when (and (TeX-active-mark)
                 (> (point) (mark)))
        (exchange-point-and-mark))
      (move-end-of-line 1)
      (newline)
      (if (assoc env LaTeX-item-list)
          (funcall (cdr (assoc env LaTeX-item-list)))
        (TeX-insert-macro "item"))
      (indent-according-to-mode)))

  ;; i don't use `electric-pair-mode' or anything similar so { should only
  ;; insert a { and not be `LaTeX-insert-left-brace' as it is by default. the
  ;; same goes for [ and (
  (defun /tex/LaTeX-mode-hook ()
    "Bindings to apply to `LaTeX-mode' when its loaded through a hook."
    (after [abbrev LaTeX]
      (abbrev-table-put latex-mode-abbrev-table
                        :regexp "\\(\\\\[a-z0-9@]+\\)"))
    (/bindings/define-keys LaTeX-mode-map
      ([remap LaTeX-insert-item] #'/tex/LaTeX-insert-item)
      ([remap LaTeX-insert-left-brace] #'self-insert-command)))

  (add-hook 'LaTeX-mode-hook #'/tex/LaTeX-mode-hook)

  ;; `pdf-view-mode' bindings
  (after 'pdf-view-mode
    (/bindings/define-key TeX-mode-map
      (kbd "C-c C-g") #'pdf-sync-forward-search)))

(provide 'lang-tex)
;;; lang-tex.el ends here
