;; Don’t require installing executables to run tests
(setq wisi-grammar-process-parse-exec (expand-file-name "./wisi_grammar_wisi_parse.exe"))

;; Override default auto-mode-alist entry for .wy
(setq auto-mode-alist (delete '("\\.wy\\'" . wisent-grammar-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.wy\\'" . wisi-grammar-mode))

(load $(WISI)/build/run-indent-test.el)

(require 'run-indent-test)

;; end of file
