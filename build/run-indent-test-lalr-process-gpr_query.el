;; run tests using wisi-process-parse, gpr_query, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)
(setq ada-parser 'process)

;; Don’t require installing executables to run tests; use elpa if installed
(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))
(setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lalr_parse" ada-mode-dir))
(setq gpr-process-parse-exec (expand-file-name "gpr_mode_wisi_parse" ada-mode-dir))
(setq gpr-query-exec (expand-file-name "gpr_query" ada-mode-dir))

(setq project-find-functions '(wisi-prj-current-cached))

(require 'run-indent-test)

(cond
 ((eq debug-on-error t)
  (setq debug-on-error nil))

 ((string-equal debug-on-error "really-t")
  (setq debug-on-error t))

 (t
  (setq debug-on-error nil))
 )
(setq wisi-debug 1) ;; abort on non-syntax errors
(when (string-equal "" wisi-parser-verbosity)(setq wisi-parser-verbosity "debug=1"));; enable wisitoken.debug_mode

;;; end of file
