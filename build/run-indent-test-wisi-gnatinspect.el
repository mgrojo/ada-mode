;; run tests using gnat-inspect, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gnat_inspect)

(require 'run-indent-test)

(setq wisi-debug 1);; report parse errors

;;; end of file
