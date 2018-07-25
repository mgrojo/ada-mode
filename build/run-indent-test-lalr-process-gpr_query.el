;; run tests using wisi-process-parse, gpr_query, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)
(setq ada-parser 'process)

;; Don’t require installing executables to run tests
(setq ada-process-parse-exec (expand-file-name "../ada_mode_wisi_lalr_parse.exe"))
(setq gpr-process-parse-exec (expand-file-name "../gpr_mode_wisi_parse.exe"))

(setq wisi-test-parser 'process) ;; for non-Ada tests

(require 'run-indent-test)

;; don’t report parse errors; recover from them!
(setq debug-on-error nil)
(setq wisi-debug 0)

;;; end of file
