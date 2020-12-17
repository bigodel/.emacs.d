;;; config-pres.el --- Configuration for a presentations -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;; TODO: finish this minor mode
;;; install `org-tree-slide', the basis for it
(require-package 'org-tree-slide)

(defun org-toggle-hide-emphasis-markers ()
  "Toggle the variable `org-hide-emphasis-markers'."
  (interactive)
  (when (boundp 'org-hide-emphasis-markers)
    (if org-hide-emphasis-markers
        (setvar 'org-hide-emphasis-markers t 'local)
      (setvar 'org-hide-emphasis-markers nil 'local))))

;;; the minor mode
;; * this might be working with `demo-it' or not, still got to work with `demo-it'
;;   to think of something
;; * hide markers in org mode (including =text=)
;; * maybe hide the modeline
;; * disable display-line-numbers-mode and display-fill-column-indicator-mode if
;;   active
(define-minor-mode presentation-mode
  "A minor mode for presenting stuff with `org-tree-slide'."
  :init-value nil
  :global nil
  :lighter " Presentation"
  :keymap '(("")))

(provide 'config-pres)
;;; config-pres.el ends here
