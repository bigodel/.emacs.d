;;; bindings-ivy.el --- Ivy/Counsel/Swiper bindings -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:
;;
;; This was mostly done by Bailey Ling. You can find all of Bailey Lings' Emacs
;; configuration in https://github.com/bling/dotemacs.
;;
;;; Code:
;; counsel loads ivy and swiper
(after 'counsel
  ;;; escape quits ivy
  (bindings-define-key ivy-mode-map [escape] (kbd "C-g"))

  ;; ivy minibuffer map
  (bindings-define-keys ivy-minibuffer-map
    ((kbd "M-m") #'ivy-mark)
    ((kbd "M-u") #'ivy-unmark))

  ;;; global map
  (bindings-define-keys (current-global-map)
    ((kbd "C-s") #'swiper)
    ((kbd "C-S-s") #'swiper-all)
    ((kbd "C-c C-r") #'ivy-resume "ivy-resume")
    ((kbd "M-y") #'counsel-yank-pop)    ; evil changes this, so i add it here
    ((kbd "C-c c L") #'counsel-load-library "load library")
    ((kbd "C-c c P") #'counsel-package)
    ((kbd "C-c c f") #'counsel-find-library "find library")
    ((kbd "C-c c T") #'counsel-load-theme "load theme")
    ((kbd "C-c c h") #'counsel-command-history "command history")
    ((kbd "C-c c C") #'counsel-colors-emacs "colors emacs")
    ((kbd "C-c c c") #'counsel-colors-web "colors web")
    ((kbd "C-c c l") #'counsel-locate "locate")
    ((kbd "C-c c o") #'counsel-outline "jump to outline")
    ((kbd "C-c c s g") #'counsel-grep-or-swiper "grep")
    ((kbd "C-c c s i") #'counsel-git-grep "git grep")
    ((kbd "C-c c s a") #'counsel-ag)
    ((kbd "C-c c s p") #'counsel-pt)
    ((kbd "C-c c s r") #'counsel-rg)
    ((kbd "C-c c C-t") #'ivy-tasks))    ; our custom function for TODO's

  ;;; space map
  (bindings-define-key bindings-space-map
    (kbd "C-t") #'ivy-tasks)

  ;;; try some searchers to see which to use; default to grep
  (cond
   ((executable-find "rg")              ; rg
    (bindings-define-key (current-global-map)
      (kbd "C-c c g") #'counsel-rg))
   ((executable-find "ag")              ; ag
    (bindings-define-key (current-global-map)
      (kbd "C-c c g") #'counsel-ag))
   ((executable-find "pt")              ; pt
    (bindings-define-key (current-global-map)
      (kbd "C-c c g") #'counsel-pt))
   (t                                   ; default
    (bindings-define-key (current-global-map)
      (kbd "C-c c g") #'counsel-grep)))

  ;;; `counsel-tramp' doesn't get loaded, so i check if the package is installed
  (after "counsel-tramp-autoloads"
    (bindings-define-key (current-global-map)
      (kbd "C-c c t") #'counsel-tramp))

  (after 'evil
    (evil-define-key '(normal visual motion) 'global
      "/" #'swiper)))

(provide 'config-bindings-ivy)
;;; bindings-ivy.el ends here
