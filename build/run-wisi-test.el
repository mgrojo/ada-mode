;; Run as:
;;
;; $(EMACS) -Q -batch -l run-wisi-test.el --eval (run-test "<filename>")
;;

(require 'wisi-parse)

(defun run-test-here (filename)
  ;; split out from run-test for interactive debugging
  (interactive "Mgrammar filename: ")
  (let ((parse-table (symbol-value (intern-soft (concat filename "-wy--parse-table"))))
	(wisent-parse-max-stack-size 15))
    (wisi-setup
     nil ;; indent-calculate
     (symbol-value (intern-soft (concat filename "-wy--keyword-table")))
     (symbol-value (intern-soft (concat filename "-wy--token-table")))
     parse-table)
    (setq wisi-test-success nil)
    (wisi-parse parse-table 'wisi-forward-token)
    (unless wisi-test-success
      (error "failed"))
  ))

(defun run-test (filename)
  (require (intern (concat filename "-wy")))
  ;; top level parse action must set `wisi-test-success' t.
  (let ((input-file (concat "../../test/wisi/" filename ".input")))
    (unless (file-readable-p input-file)
      (error "%s not found" input-file))
    (find-file input-file)
    (run-test-here filename)
    (with-temp-buffer
      (insert "success")
      (write-file (concat filename ".wisi-test")))
  ))

;; end of file
