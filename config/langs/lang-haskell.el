(require-package 'haskell-mode)

(/boot/lazy-major-mode "\\.ghci\\'" 'haskell-mode)

(when (maybe-require-package 'intero)
  (after 'haskell-mode
    (intero-global-mode)
    (add-hook 'haskell-mode-hook 'subword-mode)
    (add-hook 'haskell-mode-hook 'eldoc-mode))
  (after 'haskell-cabal
    (add-hook 'haskell-cabal-mode 'subword-mode)
    (define-key haskell-cabal-mode-map (kbd "C-c C-l") 'intero-restart))
  (after [intero flycheck]
    (flycheck-add-next-checker 'intero
                               '(warning . haskell-hlint))))
