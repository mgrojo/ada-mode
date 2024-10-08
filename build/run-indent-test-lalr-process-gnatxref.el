;; run tests using wisi-process-parse, gnatxref, otherwise ada-mode, gpr-mode defaults

(setq ada-xref-backend 'gnat)

;; Don’t require installing executables to run tests; use elpa if installed
(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))
(setq ada-process-parse-exec (expand-file-name "bin/ada_mode_wisi_lalr_parse" ada-mode-dir))

(setq gpr-query-dir (file-name-directory (locate-file "gpr-query.el" load-path)))
(setq gpr-query-exec (expand-file-name "bin/gpr_query" gpr-query-dir))

(setq project-find-functions '(wisi-prj-current-cached))

(setq ada-gnat-debug-run t)

(require 'run-indent-test)

(setq debug-on-error nil)
(setq wisi-debug 1);; enable wisitoken.debug_mode, abort on non-syntax errors

;;; end of file
