;; run tests using gnat-inspect, otherwise ada-mode, gpr-mode defaults

;; Some gnat-inspect functions invoke `compilation-start' to run
;; "gnatinspect" in a background process to produce a list of
;; references. That fails for some reason in batch mode; the
;; background process never runs. Unbinding `start-process' causes
;; `compilation-start' to run "gnatinspect" synchronously in the
;; foreground process. That also means we don't have to guess at how
;; long to wait for the results.
(fmakunbound 'start-process)

(require 'gnat-inspect)
(require 'ada-wisi-opentoken)

(require 'run-indent-test)

;;; end of file
