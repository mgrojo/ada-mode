;; run tests using wisi-ext-parse, gpr_query, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)
(setq wisi-parser 'ada)
;; ada_mode_wisi_parse.exe must be installed in PATH

(require 'run-indent-test)

(setq debug-on-error t)
(setq wisi-debug 1);; report parse errors

;;; end of file
