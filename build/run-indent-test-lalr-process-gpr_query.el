;; run tests using lalr process parse, gpr_query, otherwise ada-mode,
;; gpr-mode defaults. partial vs incremental parse not specified; see
;; run-indent-test-lalr-[partial|incremental]-gpr_query.el

(setq ada-xref-tool 'gpr_query)
(setq ada-parser 'process)

(require 'run-indent-test)

;; Don’t require installing executables to run tests; use elpa if installed
;; Must be after (package-initialize) in run-indent-test for installed elpa
(setq ada-mode-dir (file-name-directory (locate-file "ada-mode.el" load-path)))
(setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lalr_parse" ada-mode-dir))
(setq gpr-process-parse-exec (expand-file-name "gpr_mode_wisi_lr1_parse" ada-mode-dir))
(setq gpr-query-exec (expand-file-name "gpr_query" ada-mode-dir))

(setq project-find-functions '(wisi-prj-current-cached))

(provide 'run-indent-test-lalr-process-gpr_query)
;;; end of file
