;; run tests using wisi-process-parse, gpr_query, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)
(setq ada-parser 'process)
;; ada_mode_wisi_parse.exe must be installed in PATH

(require 'run-indent-test)

(setq debug-on-error t)

(setq wisi-debug 0);; don’t report parse errors; recover from them!

;;; end of file
