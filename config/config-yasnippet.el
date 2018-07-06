(/boot/delayed-init
 (require-package 'yasnippet)

 (require 'yasnippet)

 (setq yas-fallback-behavior 'return-nil)
 (setq yas-also-auto-indent-first-line t)
 (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

 (yas-load-directory (concat user-emacs-directory "snippets")))

 (yas-global-mode)

(provide 'config-yasnippet)
