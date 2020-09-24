;;; config-web.el --- Web related configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; installation of modes for different types of web files
(lazy-major-mode "\\.jade\\'" 'jade-mode)
(lazy-major-mode "\\.scss\\'" 'scss-mode)
(lazy-major-mode "\\.sass\\'" 'sass-mode)
(lazy-major-mode "\\.less\\'" 'less-css-mode)

;;; `coffee-mode'
(lazy-major-mode "\\.coffee\\'" 'coffee-mode)
(setvar 'coffee-indent-like-python-mode t)

;;; `emmet-mode'
(defun web-turn-on-emmet-mode ()
  "Install (if not installed) and enable `emmet-mode'."
  (require-package 'emmet-mode)
  (emmet-mode))

(add-hook 'css-mode-hook #'web-turn-on-emmet-mode)
(add-hook 'sgml-mode-hook #'web-turn-on-emmet-mode)
(add-hook 'web-mode-hook #'web-turn-on-emmet-mode)

;;; use `rainbow-mode'
(after 'rainbow-mode
  (add-hook 'html-mode-hook #'rainbow-mode)
  (add-hook 'web-mode-hook #'rainbow-mode)
  (add-hook 'css-mode-hook #'rainbow-mode)
  (add-hook 'stylus-mode-hook #'rainbow-mode))

;;; `web-mode' installation
(lazy-major-mode "\\.html?$" 'web-mode)
(lazy-major-mode "\\.phtml\\'" 'web-mode)
(lazy-major-mode "\\.tpl\\.php\\'" 'web-mode)
(lazy-major-mode "\\.[agj]sp\\'" 'web-mode)
(lazy-major-mode "\\.as[cp]x\\'" 'web-mode)
(lazy-major-mode "\\.erb\\'" 'web-mode)
(lazy-major-mode "\\.mustache\\'" 'web-mode)
(lazy-major-mode "\\.djhtml\\'" 'web-mode)

;;; `web-mode' configuration
(after 'web-mode
  (defun web-hook ()
    "Setup `web-mode'.
Deactivate `electric-pair-mode', because `web-mode' already
handles it.

Activate `lsp'.

Only enable auto-quoting if not in a JSX file."
    (electric-pair-mode -1)

    (lsp)

    (setvar 'web-mode-enable-auto-quoting
            (not (equal web-mode-content-type "jsx"))))
  (add-hook 'web-mode-hook #'web-hook)

  ;;; active `yasnippet' on `web-mode'
  (after 'yasnippet
    (add-hook 'web-mode-hook #'yas-minor-mode))

  ;;; add js linting to flycheck on `web-mode'
  (after 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  ;;; variables
  ;; indent offset
  (setvar 'web-mode-code-indent-offset 2) ; for code
  (setvar 'web-mode-markup-indent-offset 2) ; markup
  (setvar 'web-mode-css-indent-offset 2)    ; CSS
  (setvar 'web-mode-sql-indent-offset 2)    ; and SQL

  ;; other variables
  (setvar 'web-mode-enable-auto-pairing t)
  (setvar 'web-mode-enable-current-column-highlight t)
  (setvar 'web-mode-enable-current-element-highlight t)
  (setvar 'web-mode-enable-element-content-fontification t)
  (setvar 'web-mode-enable-element-tag-fontification t)
  (setvar 'web-mode-enable-html-entities-fontification t)
  (setvar 'web-mode-enable-inlays t)
  (setvar 'web-mode-enable-sql-detection t)
  (setvar 'web-mode-enable-block-face t)
  (setvar 'web-mode-enable-part-face t))

;;; active `lsp' on `css-mode'
(after 'config-lsp
  (add-hook 'css-mode-hook #'lsp-init))

;;; config-web.el ends here
(provide 'config-web)
