;;; config-eyecandy.el --- Some eyecandy -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; disable bars to have a as clean as possible interface
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; (unless (display-graphic-p) (menu-bar-mode -1)) ; enable in GUI Emacs
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;;; color theme
;; the theme i'm using. if it needs download, it goes here
;; (require-package 'solarized-theme)
;; (setvar 'solarized-high-contrast-mode-line t) ; make the modeline high contrast
;; (setvar 'solarized-use-more-italic t)         ; use more italics
;; (setvar 'solarized-use-variable-pitch nil)    ; all fonts monospaced?
;; (setvar 'solarized-scale-org-headlines nil)   ; don't scale org headings
;; (setvar 'solarized-scale-outline-headlines nil) ; don't scale outline headings
;; ;; avoid all font-size changes
;; (setvar 'solarized-height-minus-1 1.0)
;; (setvar 'solarized-height-plus-1 1.0)
;; (setvar 'solarized-height-plus-2 1.0)
;; (setvar 'solarized-height-plus-3 1.0)
;; (setvar 'solarized-height-plus-4 1.0)
;; (require-package 'zenburn-theme)
(load-theme 'wombat t)
;; make fringe same background color as line-number face
;; (when (version<= "26" emacs-version)
;;   (set-face-background 'fringe (face-background 'line-number)))

;; disable the bigger scale on bold function fonts (manoj-dark & misterioso)
;; (set-face-attribute 'font-lock-function-name-face nil :height 1.0)

;; make comments DimGrey (manoj-dark and default)
;; (set-face-foreground 'font-lock-comment-face "DimGrey")
;; (set-face-foreground 'font-lock-comment-delimiter-face "DimGrey")

;; make comment grey60 (misterioso)
;; (set-face-foreground 'font-lock-comment-face "grey60")
;; (set-face-foreground 'font-lock-comment-delimiter-face "grey60")

;; some customizations to the color of matching parenthesis
;; (after 'paren
;;   (set-face-foreground 'show-paren-match (face-background 'default))
;;   (set-face-background 'show-paren-match (face-foreground 'default)))

;; some customizations to the color of whitespace
(after 'whitespace
  (set-face-attribute 'whitespace-line nil
                      :background "grey30"
                      :foreground nil)) ; keep syntax highlighting

;; some customizations to the color of the highlighted line
;; keep colors when on `hl-line'
(set-face-attribute 'hl-line nil
                    :inherit nil
                    :background (face-background 'highlight))

;; i don't like these defaults because of colorblindness (mostly lsp)
;; (set-face-foreground 'error "OrangeRed")
;; manoj-dark
;; (set-face-foreground 'warning "blue")
;; (set-face-foreground 'success "DarkGreen")
;; misterioso
;; (set-face-foreground 'warning "DeepSkyBlue")
;; (set-face-foreground 'success "YellowGreen")

;; a custom theme to run on top of the other custom themes loaded (so it should
;; be here, after (load-theme 'blah)) that shows the name of the host when in
;; using tramp in the modeline alongside the buffer name. see
;; `tramp-theme-face-remapping-alist' for customization options
(require-package 'tramp-theme)
(load-theme 'tramp t)

;;; modeline
(require-package 'doom-modeline)
;; function to check if a font is installed
(defun eyecandy-font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))
;; if all the icons is not installed, install it
(when (and (not (eyecandy-font-installed-p "all-the-icons"))
           (window-system))
  (all-the-icons-install-fonts t))
(doom-modeline-mode t)
;; variables
(setvar 'doom-modeline-checker-simple-format nil) ; show one number on flycheck
(setvar 'doom-modeline-vcs-max-length 20)         ; max length of branch name
(setvar 'doom-modeline-github nil)      ; display the GitHub notifications
(setvar 'doom-modeline-icon t)   ; force icons (they are disabled with --daemon)
(setvar 'doom-modeline-modal-icon nil)  ; i'd rather have the tag from evil
(setvar 'doom-modeline-buffer-file-name-style ; how to show buffer name
        'truncate-with-project)
;; (setvar 'doom-modeline-project-detection 'project) ; default way to find project
;; (setvar 'doom-modeline-buffer-file-name-style 'relative-to-project)
;; (setvar 'doom-modeline-indent-info t)           ; display indent info

;; change mode-line's face (manoj-dark)
;; (set-face-attribute 'mode-line nil :height 1.0 :underline nil) ;
;; (set-face-attribute 'mode-line-buffer-id nil :height 1.0)
;; (set-face-attribute 'mode-line-inactive nil :height 1.0)

;; modeline indicators
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; if using git, strip the backend string from the mode line
;; (setcdr (assq 'vc-mode mode-line-format)
;;         '((:eval (replace-regexp-in-string "^ Git" " " vc-mode))))

;;; fonts
;; this is how it has to be set so it works on the daemon mode
;; or configure it on the XResources file
;; (add-to-list 'default-frame-alist '(font . "monospace-13"))
(set-frame-font "monospace-13" t)

;; tooltip font
(set-face-attribute 'tooltip nil :font "Sans Serif-13")

;;; `display-line-numbers-mode'
(defconst eyecandy-line-numbers-disabled-modes
  '(eshell-mode
    woman-mode
    Man-mode
    helpful-mode
    help-mode
    treemacs-mode
    dired-mode
    term-mode
    doc-view-mode
    pdf-view-mode
    lsp-ui-doc-frame-mode
    lsp-ui-imenu-mode
    magit-status-mode
    org-agenda-mode
    man-mode
    pomidor-mode
    proof-goals-mode
    proof-response-mode
    calc-mode
    calc-trail-mode
    undo-tree-visualizer-mode
    ibuffer-mode)
  "Modes to disable `display-line-numbers-mode'.")

(when (fboundp 'display-line-numbers-mode)
  (setvar 'display-line-numbers t)
  (setvar 'display-line-numbers-current-absolute t)

  (add-hook-to-modes eyecandy-line-numbers-disabled-modes
                     (lambda ()
                       "Disable `display-line-numbers-mode'."
                       (display-line-numbers-mode -1))))

;;; hl-line
(defconst eyecandy-hl-line-enabled-modes
  '(treemacs-mode
    dired-mode
    org-agenda-mode
    package-menu-mode
    profiler-report-mode
    ibuffer-mode)
  "Modes to enable `hl-line-mode'.")

(add-hook-to-modes eyecandy-hl-line-enabled-modes #'hl-line-mode)
;;; whitespace
(setvar 'whitespace-style '(face trailing tabs tab-mark lines-tail empty))

;; I don't enable `global-whitespace-mode' because there are non file modes,
;; like `dired', in which I don't want it activated
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'whitespace-mode)

;; ws-butler is a better way to delete trailing whitespace, it deletes
;; whitespace only on the lines that have been changed. that avoids having
;; commits and such with a bunch of changes that are only whitespace removal.
;; keep the whitespace littered code of other they way they intended, ugly...
;; it only clean whitespace when saving, like the old hook we had
(require-package 'ws-butler)
(ws-butler-global-mode)

;;; misc
(setvar 'x-underline-at-descent-line t)       ; underline below font baseline

;; stop blinking cursor
(blink-cursor-mode -1)

;; hide all minor modes from mode line (not needed with doom-modeline)
;; (require-package 'rich-minority)
;; (rich-minority-mode t)
;; (setvar 'rm-blacklist "")
;; TODO: make it so that flycheck only displays erros and warnings without the
;; FlyC text and make a better way to display LSP information without the
;; TODO: use a better alternative, this is pretty memory intensive...
;; obnoxious [Disconnected] or [blablabla_server:123412]
;; only show lsp and flycheck on mode line
;; (setvar 'rm-whitelist (format "^ \\(%s\\)$"
;;                               (mapconcat #'identity
;;                                          '("FlyC.*" "LSP.*")
;;                                          "\\|")))

;; highlight TODO
(require-package 'hl-todo)
(global-hl-todo-mode t)

;; make treemacs a little bit better
(after 'treemacs
  (set-face-attribute 'treemacs-directory-collapsed-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-directory-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-file-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-fringe-indicator-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-git-added-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-git-conflict-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-git-ignored-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-git-modified-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-git-renamed-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-git-unmodified-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-git-untracked-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-help-column-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-help-title-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-on-failure-pulse-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-on-success-pulse-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-root-face nil :font "Sans Serif" :height 1.2)
  (set-face-attribute 'treemacs-root-remote-disconnected-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-root-remote-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-root-remote-unreadable-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-root-unreadable-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-tags-face nil :font "Sans Serif")
  (set-face-attribute 'treemacs-term-node-face nil :font "Sans Serif"))

(provide 'config-eyecandy)
;;; config-eyecandy.el ends here
