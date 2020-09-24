;;; config-js.el --- JavaScript configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; Most of this configuration was taken from Doom Emacs' JavaScript
;; configuration:
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/javascript
;;
;;; Code:
;;; install and use `js2-mode' on JavaScript related modes
(lazy-major-mode "\\.m?js\\'" 'js2-mode)
;; use `js2-mode' for shell scripts running via node.js
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; use `conf-unix-mode' for npmrc
(add-to-list 'auto-mode-alist '("\\.?npmrc\\'" . conf-mode) t)

;;; `js-mode' variables
(setvar 'js-indent-level 2)             ; how many spaces
(setvar 'js-chain-indent t)             ; line up things with .

;;; `js2-mode' configuration
(after 'js2-mode
  ;; interpret lines starting with # as comments
  (setvar 'js2-skip-preprocessor-directives t)
  ;; maximum fontification
  (setvar 'js2-highlight-level 3)

  ;; let flycheck handle this
  (after 'flycheck
    (setvar 'js2-mode-show-parse-errors nil)
    (setvar 'js2-mode-show-strict-warnings nil)
    ;; flycheck provides these features, so disable them: conflicting with
    ;; the eslint settings.
    (setvar 'js2-strict-trailing-comma-warning nil)
    (setvar 'js2-strict-missing-semi-warning nil)))

;; JSX configuration
;; (progn
;;   ;;; `rjsx-mode' for JSX files
;;   (lazy-major-mode "components/.+\\.js$" 'rjsx-mode)

;;   (defun js-jsx-file-p ()
;;     "Detect React or preact imports early in the file."
;;     (interactive)
;;     (and buffer-file-name
;;          (string= (file-name-extension buffer-file-name) "js")
;;          (re-search-forward "\\(^\\s-*import +React\\|\\( from \\|require(\\)[\"']p?react\\)"
;;                             magic-mode-regexp-match-limit t)
;;          (progn (goto-char (match-beginning 1))
;;                 ;; detect if point is not inside a string or comment
;;                 (not (or (nth 3 (syntax-ppss)) ; point in string
;;                          (nth 4 (syntax-ppss))))))) ; point in comment
;;   ;; using `magic-mode-alist' instead of `auto-mode-alist' allows us to
;;   ;; override the former in the case where matching function is not nil. it
;;   ;; it is nil then allow `auto-mode-alist' to determine the major mode
;;   (add-to-list 'magic-mode-alist
;;                '(js-jsx-file-p . (lambda ()
;;                                    (require-package 'rjsx-mode)
;;                                    (require 'rjsx-mode)
;;                                    (rjsx-mode))))
;;   ;;; `rjsx-mode' configuration
;;   (after 'rjsx-mode
;;     (after 'flycheck
;;       ;; jshint doesn't know how to deal with jsx
;;       (add-to-list 'javascript-jshint 'flycheck-disabled-checkers))

;;     ;; start `lsp'
;;     (after 'config-lsp
;;       (add-hook 'rjsx-mode-hook #'lsp-init))))
;;; deal with JSX files with `web-mode'
(after 'web-mode
  ;; i associate TSX files with `js-tsx-mode' derived from `web-mode' because
  ;; `js2-mode' does not officially support JSX. see
  ;; https://github.com/mooz/js2-mode#react-and-jsx
  (define-derived-mode js-jsx-mode web-mode "JSX")
  (add-to-list 'auto-mode-alist
               '("\\.jsx\\'" . (lambda ()
                                 (require-package 'web-mode)
                                 (require 'web-mode)
                                 (js-ljsx-mode))))
  ;; use `js2-minor-mode' on our newly created mode
  (add-hook 'js-jsx-mode-hook #'js2-minor-mode)
  (add-hook 'js-jsx-mode-hook
            (lambda ()
              "Expand className instead of class on `emmet-mode'."
              (setvar 'emmet-expand-jsx-className? t 'local)))

  ;; add the newly created mode to the possible linters for flycheck
  (after 'flycheck
    (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)))

;;; `lsp'
(after 'config-lsp
  (add-to-list 'dotemacs-lsp-inhibit-paths "node_modules")

  (add-hook 'js2-mode-hook #'lsp-init)
  (add-hook 'js-jsx-mode-hook #'lsp-init))

(provide 'config-js)
;;; config-js.el ends here
