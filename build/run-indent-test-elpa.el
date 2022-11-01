;; run tests using elpa build

(setq ada-xref-backend 'gpr_query)

(require 'run-indent-test)

(setq wisi-incremental-parse-enable t)

(setq ada-mode-dir "c:/Projects/elpa_release/ada-mode/")
(setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lalr_parse" ada-mode-dir))

(setq gpr-query-dir "c:/Projects/elpa_release/ada-mode/")
(setq gpr-query-exec (expand-file-name "gpr_query" gpr-query-dir))

(setq project-find-functions '(wisi-prj-current-cached))

(provide 'run-indent-test-elpa)
;; end of file
