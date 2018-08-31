(add-to-list
 'load-path (concat user-emacs-directory "el-get/el-get"))
(add-to-list
 'load-path (concat user-emacs-directory "el-get/proof-general"))

(require-package 'el-get)
(require 'el-get)

(setq el-get-sources
      '((:name proof-general
               :type github
               :pkgname "ProofGeneral/PG"
               :url "https://github.com/ProofGeneral/PG.git")))

(unless (load (concat user-emacs-directory
                      "el-get/proof-general/generic/proof-site.el") 'noerror)

(let* ((el-get-root
        (file-name-as-directory
         (or (bound-and-true-p el-get-dir)
             (concat (file-name-as-directory user-emacs-directory)
                     "el-get"))))
       (package "proof-general")
       (buf (switch-to-buffer (format "*%s bootstrap*" package)))
       (pkgdir (file-name-as-directory (concat el-get-root package)))
       (git (or (executable-find "git")
                (error "Unable to find `git'")))
       (url "https://github.com/ProofGeneral/PG.git")
       (default-directory el-get-root)
       (process-connection-type nil)) ; use pipe instead of pty

;; clone the package (in this case, proof general)
(unless (zerop (funcall #'call-process git nil `(,buf t) 'display
                        "--no-pager" "clone" "-v" url package))
  (error "Couldn't clone \"%s\" from the Git repositorty: %s" package url))

(add-to-list 'load-path pkgdir)
(load-file (concat pkgdir "generic/proof-site.el"))
(load 'proof-site)
(insert (format "Package \"%s\" loaded!" package))

(el-get 'sync 'proof-general)))

(setq proof-strict-read-only t)
(setq proof-three-window-mode-policy 'smart)
(setq proof-indent (symbol-value 'tab-width))
(setq proof-splash-enable nil)
(setq proof-script-fly-past-comments t)

(/boot/lazy-major-mode "\\.v$" 'coq-mode)

(require 'proof-site)

(after 'proof-site
  (after 'evil
    (evil-ex-define-cmd "pr[ove]" 'proof-goto-point)
    (evil-define-key 'normal proof-mode-map (kbd "M-n")
      'proof-assert-next-command-interactive)
    (evil-define-key 'normal proof-mode-map (kbd "M-p")
      'proof-undo-last-successful-command)
    (evil-define-key 'normal proof-mode-map (kbd "C-n")
      'pg-next-input)
    (evil-define-key 'normal proof-mode-map (kbd "C-p")
      'pg-previous-input)
    (evil-define-key 'insert proof-mode-map (kbd "M-n")
      'proof-assert-next-command-interactive)
    (evil-define-key 'insert proof-mode-map (kbd "M-p")
      'proof-undo-last-successful-command)
    (evil-define-key 'insert proof-mode-map (kbd "C-n")
      'pg-next-input)
    (evil-define-key 'insert proof-mode-map (kbd "C-p")
      'pg-previous-input))

(add-hook
 'proof-mode-hook
 (lambda ()
   "Enable `undo-tree-mode', disable `holes-mode' and enable
  `flyspell-prog-mode' in Proof General's modes"
   (flyspell-prog-mode)
   (undo-tree-mode t)
   (holes-mode -1)))

(require-package 'company-coq)
  (setq company-coq-disabled-features '(prettify-symbols smart-subscripts))
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (add-hook 'company-coq-mode-hook
            (lambda ()
              (setq company-idle-delay 0.5)
              (setq coq-one-command-per-line nil))))

(el-get 'sync 'proof-general)

(provide 'init-pg)
