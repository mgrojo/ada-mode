;; run only indent, using ada_mode_gps_indent; compare ada-gps indentation to ada-wisi

(require 'ada-gps)
(require 'run-indent-test)

(setq ada-gps-exec (concat default-directory "../ada_mode_gps_indent"))
(setq ada-gps-size-threshold 0)
(setq skip-cmds t)
(setq skip-recase-test t)

;;; end of file
