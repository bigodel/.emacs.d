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

  (after 'projectile
    ;; make projectile aware of dart projects
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")

    ;; ignore some dart and flutter specific directories
    (add-to-list 'projectile-globally-ignored-directories dart-sdk-path)
    (add-to-list 'projectile-globally-ignored-directories ".pub-cache")
    (add-to-list 'projectile-globally-ignored-directories
                 (if (null (getenv "FLUTTER_SDK"))
                     (expand-file-name "proj/flutter" (getenv "HOME"))
                   (getenv "FLUTTER_SDK"))))

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

    (after 'config-lsp
      (add-to-list 'dotemacs-lsp-inhibit-paths dart-sdk-path)
      (add-to-list 'dotemacs-lsp-inhibit-paths ".pub-cache")
      (add-to-list 'dotemacs-lsp-inhibit-paths
                   (if (null (getenv "FLUTTER_SDK"))
                       (expand-file-name "proj/flutter" (getenv "HOME"))
                     (getenv "FLUTTER_SDK")))

      (add-hook 'dart-mode-hook #'lsp-init))

    ;; setup `dap-mode'
    (dap-dart-setup)))

(provide 'config-dart)
;;; config-dart.el ends here
