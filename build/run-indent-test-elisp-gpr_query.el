;; run Ada tests using gpr_query, elisp parser ada-mode, gpr-mode defaults

(setq ada-xref-tool 'gpr_query)
(setq ada-parser 'elisp)
(setq gpr-parser 'elisp)

(require 'run-indent-test)

(setq wisi-debug 1);; report parse errors
;;(setq debug-on-error t)

;;; end of file
