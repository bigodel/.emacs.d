
(setq-default c-basic-offset (symbol-value 'tab-width))

(add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))

(after 'company
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-clang-arguments '("-I../"
                                  "-I./"))

  (require-package 'company-c-headers)
  (require 'company-c-headers)
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/v1/")
  (add-to-list 'company-c-headers-path-system "/usr/local/include/c++/4.9.4/"))
