(after 'evil
  (/bindings/define-keys evil-normal-state-map ("g d" #'dumb-jump-go))

  (require-package 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

  (after "evil-numbers-autoloads"
    (/bindings/define-key evil-normal-state-map "C-a" #'evil-numbers/inc-at-pt)
    (/bindings/define-key evil-normal-state-map "C-S-a" #'evil-numbers/dec-at-pt))

  (after "company-autoloads"
    (/bindings/define-keys evil-insert-state-map
      ("C-n" #'company-complete)
      ("TAB" #'company-indent-or-complete-common)))

  (after 'proof-site
    (evil-ex-define-cmd "pr[ove]" 'proof-goto-point)
    (evil-define-key 'normal proof-mode-map (kbd "M-n")
      'proof-assert-next-command-interactive)
    (evil-define-key 'normal proof-mode-map (kbd "M-p")
      'proof-undo-last-successful-command)
    (evil-define-key 'insert proof-mode-map (kbd "M-n")
      'proof-assert-next-command-interactive)
    (evil-define-key 'insert proof-mode-map (kbd "M-p")
      'proof-undo-last-successful-command))

  (/bindings/define-keys evil-normal-state-map
    ("SPC" ":noh")
    ("C-b" #'evil-scroll-up)
    ("C-f" #'evil-scroll-down))

  (after 'evil-evilified-state
    (/bindings/define-keys evil-evilified-state-map
      ("C-w h" #'evil-window-left)
      ("C-w j" #'evil-window-down)
      ("C-w h" #'evil-window-up)
      ("C-w l" #'evil-window-right)))

  (/bindings/define-keys evil-normal-state-map
    ("C-w h" #'evil-window-left)
    ("C-w j" #'evil-window-down)
    ("C-w k" #'evil-window-up)
    ("C-w l" #'evil-window-right))

  (/bindings/define-key evil-normal-state-map "Y" "y$"))

(provide 'init-bindings-evil)
