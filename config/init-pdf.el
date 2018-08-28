(/boot/delayed-init
 (require-package 'pdf-tools)
 (require 'pdf-tools)

 (unless (pdf-info-running-p)
   (pdf-tools-install)))

;; add this in the LaTeX part later to revert the pdf after TeX finished
;; compiling
;; (add-hook 'TeX-after-compilation-finished-functions
;;           #'TeX-revert-document-buffer)
