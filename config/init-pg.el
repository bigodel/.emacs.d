(add-to-list
 'load-path (concat user-emacs-directory "el-get/el-get"))

(require-package 'el-get)
(require 'el-get)

(setq el-get-sources
      '((:name proof-general
               :type github
               :pkgname "ProofGeneral/PG"
               :url "https://github.com/ProofGeneral/PG.git")))

(unless (require 'proof-site nil t)
  (unless (file-directory-p "~/.emacs.d/el-get")
    (make-directory "~/.emacs.d/el-get"))
  (shell-command
   "git clone https://github.com/ProofGeneral/PG.git ~/.emacs.d/el-get/proof-general")
  (el-get 'sync 'proof-general))

(setq proof-strict-read-only t)
(setq proof-electric-terminator-enable t)
(setq proof-indent (symbol-value 'tab-width))
(setq proof-splash-time 4)
(setq proof-splash-enable nil)

(require
 'proof-site (concat user-emacs-directory
                     "el-get/proof-general/generic/proof-site"))

(after 'proof-site
  (require-package 'company-coq)

  ;; for some reason proof mode deactivates undo-tree
  ;; and I don't like the holes
  (add-hook
   'proof-mode-hook
   '(lambda ()
      "Enable `undo-tree-mode' and disable `holes-mode' in Proof
        General's modes"
      (undo-tree-mode t)
      (holes-mode -1))))

(el-get 'sync 'proof-general)
