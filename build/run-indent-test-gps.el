;; run only indent, using ada_mode_gps_indent; compare ada-gps indentation to ada-wisi

(package-initialize)
(require 'ada-gps)
(require 'run-indent-test)

(setq ada-gps-exec (concat default-directory "../ada_mode_gps_indent"))
(setq ada-gps-size-threshold 0)
(setq skip-recase-test t)

;;; end of file
