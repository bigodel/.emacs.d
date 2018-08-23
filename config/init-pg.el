(require 'proof-site "~/.emacs.d/elisp/PG/generic/proof-site")
(setq proof-strict-read-only t)
;; look into electric-terminator for coq
;; maybe activate tool-bar-mode only when on proof general (also activating
;; the toolbar with M-x proof-toolbar-toggle or C-c b)

;; for some reason coq mode deactivates undo-tree
(add-hook 'coq-mode-hook 'turn-on-undo-tree-mode)

(after 'evil
  (evil-ex-define-cmd "pr[ove]" 'proof-goto-point)
  (evil-define-key 'normal coq-mode-map (kbd "C-n")
    'proof-assert-next-command-interactive)
  (evil-define-key 'normal coq-mode-map (kbd "C-p")
    'proof-undo-last-successful-command))

;; proof general has a unicode symbols that shows a big sans true and false
;; and I didn't like it so I changed it to the default font and normal size
;; (set-face-attribute
;;   'unicode-tokens-sans-font-face
;;   nil :height 1.0 :family '(face-attribute 'default :font)))
