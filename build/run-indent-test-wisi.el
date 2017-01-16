;; run tests using ada-wisi, gpr-wisi, ada-gnat-xref

;; ada-mode, gpr-mode default to ada-wisi, gpr-wisi
;; ada-xref-tool defaults to gpr_query if that is in path
;; ada-gps is fallback indent if ada_mode_gps_indent is in path

(setq ada-xref-tool 'gnat)

(require 'run-indent-test)

(setq wisi-debug 1);; report parse errors

;;; end of file
