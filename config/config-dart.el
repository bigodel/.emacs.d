;;; config-dart.el --- Dart configuration

;; Author: João Pedro de Amorim Paula <maybe_add_email@later>

;;; Commentary:

;;; Code:
;;; install `dart-mode' only if we visit a dart file
(lazy-major-mode "\\.dart\\'" dart-mode)

;;; `dart-mode' configuration
(after 'dart-mode
  ;; TODO: add a way to automatically install dart sdk and probably flutter
  (setvar 'dart-sdk-path                ; the path for the sdk
          (if (null (getenv "DART_SDK"))
              (concat (getenv "HOME") "/dev/flutter/bin/cache/dart-sdk")
            (getenv "DART_SDK")))
  (setvar 'dart-format-on-save t)       ; use dartfmt to format code on save

  ;; make projectile aware of dart projects
  (after 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

  (require-package 'flutter)            ; we use dart with flutter
  (setvar 'flutter-sdk-path             ; path to flutter sdk
          (if (null (getenv "FLUTTER_SDK"))
              (concat (getenv "HOME") "/dev/flutter")
            (getenv "FLUTTER_SDK")))

  ;; start LSP when on dart files. if our configuration for lsp is available,
  ;; use the function that we defined, otherwise if lsp is installed, start it
  (if (featurep 'config-lsp)
      (progn
        ;; dart configuration regarding lsp
        (setvar 'lsp-dart-suggest-from-unimported-libraries t)
        (setvar 'lsp-dart-sdk-dir dart-sdk-path)
        (add-hook 'dart-mode-hook #'lsp-start))
    (after "lsp-mode-autoloads"
      ;; dart configuration regarding lsp
      (setvar 'lsp-dart-suggest-from-unimported-libraries t)
      (setvar 'lsp-dart-sdk-dir dart-sdk-path)
      (add-hook 'dart-mode-hook #'lsp))))

(provide 'config-dart)
;;; config-dart.el ends here
