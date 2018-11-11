
(setq TeX-brace-indent-level 4)
(setq LaTeX-indent-level 4)
(setq tex-fontify-script nil)
(setq font-latex-fontify-script nil)
(setq TeX-auto-save t)
(setq TeX-view-program-selection
      '((output-pdf "Zathura")
        (output-dvi "xdvi")))
;; compile to pdf by default
(setq TeX-pdf-mode t)

(require-package 'auctex)
(require-package 'company-auctex)

(company-auctex-init)

(after 'auctex
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))
