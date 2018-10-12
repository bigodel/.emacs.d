
(require-package 'pdf-tools)
(add-hook 'doc-view-mode-hook
          (lambda ()
            "Lambda function to use `pdf-tools' in
          `doc-view-mode' when in a pdf file."
            (require 'pdf-tools)
            (pdf-view-mode)))
