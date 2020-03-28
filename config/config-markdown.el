;;; config-markdown.el --- Markdown configurations.

;; Author: João Pedro de Amorim Paula <maybe_add_later@gmail.com>

;;; Commentary:
;;
;; Configuration for markdown.
;;
;;; Code:
;;; install `markdown-mode'
(lazy-major-mode "\\.md\\'" markdown-mode)
(lazy-major-mode "\\.markdown\\'" markdown-mode)
(lazy-major-mode "README\\.md\\'" gfm-mode)

(after 'markdown-mode
  ;;; variables
  (setvar 'markdown-italic-underscore t) ; use _ instead of * for italic
  (setvar 'markdown-enable-math t)       ; syntax highlighting for inline LaTeX
  (setvar 'markdown-asymmetric-header t) ; place header only on the left side
  (setvar 'markdown-fontify-code-blocks-natively t)) ; fontify code blocks

(provide 'config-markdown)
;;; config-markdown.el ends here
