(require-package 'which-key)
(setq which-key-idle-delay 0.2)
(setq which-key-min-display-lines 3)
(which-key-mode)

(defmacro /bindings/define-prefix-keys (keymap prefix &rest body)
  (declare (indent defun))
  `(progn
     ,@(cl-loop for binding in body
                collect
                `(let ((seq ,(car binding))
                       (func ,(cadr binding))
                       (desc ,(caddr binding)))
                   (define-key ,keymap (kbd seq) func)
                   (when desc
                     (which-key-add-key-based-replacements
                       (if ,prefix
                           (concat ,prefix " " seq)
                         seq)
                       desc))))))

(defmacro /bindings/define-keys (keymap &rest body)
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil ,@body))

(defmacro /bindings/define-key (keymap sequence binding &optional description)
  (declare (indent defun))
  `(/bindings/define-prefix-keys ,keymap nil
     (,sequence ,binding ,description)))

;; escape minibuffer
(define-key minibuffer-local-map [escape] '/utils/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] '/utils/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] '/utils/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] '/utils/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] '/utils/minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(global-set-key (kbd "C-c d") #'dired-sidebar-toggle)

(after "expand-region-autoloads"
  (global-set-key (kbd "C-=") 'er/expand-region))

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (bind (scroll-down 1)))
  (global-set-key [mouse-5] (bind (scroll-up 1))))

(after 'compile
  (define-key compilation-mode-map (kbd "j") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "k") 'compilation-previous-error))

(after 'helm
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-x h") #'helm-command-prefix)
  (global-set-key (kbd "C-h a") #'helm-apropos)
  (global-set-key (kbd "C-x b") #'helm-buffers-list)
  (global-set-key (kbd "C-x C-b") #'helm-mini)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x r b") #'helm-bookmarks)
  (global-set-key (kbd "C-p") #'helm-projectile)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "M-:") #'helm-eval-expression-with-eldoc)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action))

(global-set-key (kbd "M-!") 'eshell-command)
(global-set-key (kbd "C-!") '/eshell/new-window)

(after 'evil
  (define-key evil-normal-state-map (kbd "!") '/eshell/new-window)
  (define-key evil-visual-state-map (kbd "!") '/eshell/new-window)
  (define-key evil-motion-state-map (kbd "!") '/eshell/new-window))

(/bindings/define-keys (current-global-map)
  ("C-c c" #'org-capture)
  ("C-c a" #'org-agenda)
  ("C-c l" #'org-store-link)
  ("C-c s" #'/utils/goto-scratch-buffer)
  ("C-c e" #'/utils/eval-and-replace)
  ("C-c t" #'/eshell/new-split))

(/bindings/define-keys (current-global-map)
  ("C-x c" #'calculator)
  ("C-x C" #'calendar)
  ("C-x C-k" #'kill-this-buffer)
  ("C-x p" #'proced))

(/bindings/define-keys (current-global-map)
  ("C-s"   #'isearch-forward-regexp)
  ("C-M-s" #'isearch-forward)
  ("C-r"   #'isearch-backward-regexp)
  ("C-M-r" #'isearch-backward))

(global-set-key (kbd "<M-f7>") (bind (profiler-start 'cpu+mem)))
(global-set-key (kbd "<M-f6>") (bind (profiler-report) (profiler-stop)))

(provide 'config-bindings)
