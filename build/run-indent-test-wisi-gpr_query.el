;; run tests using gpr_query, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)

(require 'run-indent-test)

(setq wisi-debug 1);; report parse errors
;;(setq debug-on-error t)

;;; end of file
