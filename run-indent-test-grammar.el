;; Run an indent test for wisi-grammar-mode  -*- no-byte-compile: t -*-
(load "autoloads")

;; Don’t require installing executables to run tests
(setq wisi-grammar-process-parse-exec (expand-file-name "./wisi_grammar_mode_parse.exe"))

;; Override default auto-mode-alist entry for .wy
(setq auto-mode-alist (delete '("\\.wy\\'" . wisent-grammar-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.wy\\'" . wisi-grammar-mode))

(require 'run-indent-test)

;; end of file
