;;; config-dart.el --- Dart configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
(lazy-major-mode "\\.dart\\'" dart-mode)

(after 'dart-mode
  (setvar 'dart-sdk-path                ; the path for the sdk
          (if (null (getenv "DART_SDK"))
              (concat (getenv "HOME") "/git/flutter/bin/cache/dart-sdk")
            (getenv "DART_SDK")))
  (setvar 'dart-format-on-save t)       ; use dartfmt to format code on save

  ;; make projectile aware of dart projects
  (after 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

  (require-package 'flutter)            ; we use dart with flutter
  (setvar 'flutter-sdk-path             ; path to flutter sdk
          (if (null (getenv "FLUTTER_SDK"))
              (concat (getenv "HOME") "/git/flutter")
            (getenv "FLUTTER_SDK")))

  (after 'lsp-dart
    (setvar 'lsp-dart-sdk-dir dart-sdk-path))

  (add-hook 'dart-mode-hook #'lsp-start))

  (provide 'config-dart)
;;; config-dart.el ends here
