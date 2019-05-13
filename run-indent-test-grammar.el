;; Run an indent test for wisi-grammar-mode  -*- no-byte-compile: t -*-
(load "autoloads")

;; Don’t require installing executables to run tests
(setq wisi-grammar-process-parse-exec (expand-file-name "./wisi_grammar_mode_parse.exe"))

(package-initialize)
(require 'mmm-mode)

;; WORKAROUND: if we rely on mmm-global-mode, mmm is enabled by
;; post-command-hook, which is run after ’run-test’ (the command).
(add-hook 'wisitoken-grammar-mode-hook #'mmm-mode-on)

(require 'wisi-run-indent-test)

;; end of file
