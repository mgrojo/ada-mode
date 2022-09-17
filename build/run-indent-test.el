;; Ada specific utils for automating indentation and casing tests

(package-initialize) ;; for uniquify-files, installed wisi, ada-mode

(require 'wisi-run-indent-test)

;;; Settings for all tests; can be overridden on make command line via
;;; ELISP, or in file via EMACSCMD or Local Variables.

(setq gpr-mode-dir (file-name-directory (locate-file "gpr-mode.el" load-path)))
(setq gpr-process-parse-exec (expand-file-name "gpr_mode_wisi_parse" gpr-mode-dir))

(setq wisi-incremental-parse-enable t)

(setq debug-on-error nil)
(setq-default wisi-parser-verbosity "debug=1")
(setq-default compare-tree-text t)
(setq-default wisi-process-time-out 30.0) ;; running with debug/assert is slow

(setq wisi-debug 1) ;; abort on non-syntax errors

(provide 'run-indent-test)
;; end of file
