;; run tests using wisi-process-parse packrat, gpr_query, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)
(setq ada-parser 'process)

;; Don’t require installing executables to run tests
(setq ada-process-parse-exec (expand-file-name "../ada_mode_wisi_parse_packrat.exe"))
(setq gpr-process-parse-exec (expand-file-name "../gpr_mode_wisi_parse_packrat.exe"))

(require 'run-indent-test)

;; don’t report parse errors; recover from them!
(setq debug-on-error nil)
(setq wisi-debug 0)

;;; end of file
