;; Test ada_mode_gps_indent; compare ada-gps indentation to ada-wisi
;; Also runs face, navigate tests, using process parse

(package-initialize)
(require 'ada-gps)
(require 'run-indent-test)

;; Don’t require installing executables to run tests
(setq ada-gps-exec (expand-file-name "../ada_mode_gps_indent"))
(setq ada-process-parse-exec (expand-file-name "../ada_mode_wisi_parse.exe"))

(setq ada-gps-size-threshold 0)
(setq skip-recase-test t)

;;; end of file
