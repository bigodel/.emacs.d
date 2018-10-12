
(setq TeX-brace-indent-level 4)
(setq LaTeX-indent-level 4)
(setq tex-fontify-script nil)
(setq font-latex-fontify-script nil)

(require-package 'auctex)
(require-package 'company-auctex)

(company-auctex-init)
