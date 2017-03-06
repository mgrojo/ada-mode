;; run Ada tests using gpr_query, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)

(setq ada-gps-indent-exec "no-such-program");; FIXME: don't use gps fallback, it seems to be broken

(require 'run-indent-test)

(setq wisi-debug 1);; report parse errors
;;(setq debug-on-error t)

;;; end of file
