(/boot/delayed-init
 (require-package 'yasnippet)

 (after 'yasnippet
   (require-package 'yasnippet-snippets)
   (require-package 'yasnippet-classic-snippets))

 (require 'yasnippet)

 (setq yas-fallback-behavior 'return-nil)
 (setq yas-also-auto-indent-first-line t)
 (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

 (yas-global-mode t)

 (yas-load-directory (concat user-emacs-directory "snippets")))

(provide 'init-yasnippet)
