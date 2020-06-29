;;; config-dart.el --- Dart configuration -*- lexical-bindings: t; -*-

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install `dart-mode' only if we visit a dart file
(lazy-major-mode "\\.dart\\'" 'dart-mode)

;;; `dart-mode' configuration
(after 'dart-mode
  ;; TODO: add a way to automatically install dart sdk and probably flutter
  (setvar 'dart-sdk-path                ; the path for the sdk
          (if (null (getenv "DART_SDK"))
              (expand-file-name "proj/flutter/bin/cache/dart-sdk"
                                (getenv "HOME"))
            (getenv "DART_SDK")))
  (setvar 'dart-format-on-save t)       ; use dartfmt to format code on save

  ;; make projectile aware of dart projects
  (after 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

  ;; start LSP when on dart files. note that `lsp-dart' has `treemacs',
  ;; `lsp-treemacs' and `lsp-ui' as dependency
  (after "lsp-mode-autoloads"
    ;; install `lsp-dart'
    (require-package 'lsp-dart)
    (setvar 'lsp-dart-project-sdk-dir
            (if (boundp 'dart-sdk-path)
                dart-sdk-path
              (if (null (getenv "DART_SDK"))
                  (expand-file-name "proj/flutter/bin/cache/dart-sdk"
                                    (getenv "HOME"))
                (getenv "DART_SDK"))))
    (setvar 'lsp-dart-only-analyze-projects-with-open-files t)
    (setvar 'lsp-dart-suggest-from-unimported-libraries nil)
    (setvar 'lsp-dart-outline t)
    (setvar 'lsp-dart-flutter-outline t)
    (setvar 'lsp-dart-closing-labels t)
    (setvar 'lsp-dart-flutter-widget-guides nil)
    (setvar 'lsp-dart-flutter-fringe-colors nil)
    (setvar 'lsp-dart-test-code-lens t)
    (add-hook 'dart-mode-hook #'lsp)
    (add-hook 'dart-mode-hook (lambda ()
                                "`company' doesn't work very well
    with the Dart language server and Emacs, so I disable the idle
    delay, leaving it to autocomplete only when manually asked."
                                (setvar 'company-idle-delay nil 'local)))))

(provide 'config-dart)
;;; config-dart.el ends here
