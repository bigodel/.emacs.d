
(after 'evil
  (/bindings/define-keys evil-normal-state-map ("g d" #'dumb-jump-go))

  (require-package 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

  (after "evil-numbers-autoloads"
    (/bindings/define-key evil-normal-state-map "C-a" #'evil-numbers/inc-at-pt)
    (/bindings/define-key evil-normal-state-map "C-S-a" #'evil-numbers/dec-at-pt))

  (after 'company
    (/bindings/define-key evil-insert-state-map "<C-tab>" #'company-complete))

  (/bindings/define-keys evil-normal-state-map
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

  (/bindings/define-keys evil-motion-state-map
    ("j" #'evil-next-visual-line)
    ("k" #'evil-previous-visual-line))

  (/bindings/define-key evil-normal-state-map "Y" "y$"))

(provide 'config-bindings-evil)
