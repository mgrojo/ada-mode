;; run tests using ada-mode, gpr-mode defaults; ada-wisi, gpr-wisi, ada-gnat-xref

(require 'ada-gps)
(require 'run-indent-test)

(setq ada-gps-exec (concat default-directory "../ada_mode_gps_indent"))
(setq ada-gps-size-threshold 0) ;; compare ada-gps indentation to ada-wisi
(setq skip-cmds t)
(setq skip-recase-test t)

;;; end of file
