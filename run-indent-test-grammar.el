;; Run an indent test for wisi-grammar-mode  -*- no-byte-compile: t -*-
(load "autoloads")

;; Don’t require installing executables to run tests
(setq wisitoken-grammar-process-parse-exec (expand-file-name "./wisitoken_grammar_mode_parse.exe"))

(package-initialize)
(require 'mmm-mode)
(setq wisi-incremental-parse-enable t)

;; WORKAROUND: if we rely on mmm-global-mode, mmm is enabled by
;; post-command-hook, which is run after ’run-test’ (the command).
(add-hook 'wisitoken-grammar-mode-hook #'mmm-mode-on)

;; (setq wisi-disable-face t)

(require 'wisi-run-indent-test)

;; end of file
