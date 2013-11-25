;; run tests using gnat-inspect, otherwise ada-mode, gpr-mode defaults

;; FIXME: All gnat-inspect functions run "gnatinspect" in a background
;; process to save startup time. That fails for some reason in batch
;; mode; the background process never runs.

(require 'gnat-inspect)
(require 'ada-wisi-opentoken)

(require 'run-indent-test)

;;; end of file
