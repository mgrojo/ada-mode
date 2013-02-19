;; Run as:
;;
;; $(EMACS) -Q -batch -l run-wisi-test.el --eval (run-test "<filename>")
;;

(require 'wisi-parse)

(defun run-test-here (filename)
  ;; split out from run-test for interactive debugging
  (interactive "Mgrammar filename: ")
  (let ((parse-table (symbol-value (intern-soft (concat filename "-wy--parse-table"))))
	(wisent-parse-max-stack-size 30);; small enough to see in debugger, big enough to run tests
	(wisi-test-success nil))
    (wisi-setup
     nil ;; indent-calculate
     (symbol-value (intern-soft (concat filename "-wy--keyword-table")))
     (symbol-value (intern-soft (concat filename "-wy--token-table")))
     parse-table)

    ;; use Ada style comments in source
    (set (make-local-variable 'comment-start) "--")
    (set (make-local-variable 'comment-end) "")
    (set (make-local-variable 'comment-start-skip) "---*[ \t]*")
    (set (make-local-variable 'comment-multi-line) nil)

    (goto-char (point-min))
    (wisi-parse parse-table 'wisi-forward-token)
    (unless wisi-test-success
      (error "failed")))

  ;; check action results; --CLASS: <form> compares cached class on next token to <form>
  (let (expected-class found-class cache error-p)
    (goto-char (point-min))
    (while (re-search-forward "--CLASS:" nil t)
      (setq expected-class (eval (buffer-substring-no-properties (point) (end-of-line-position))))
      (forward-comment 1)
      (setq cache (wisi-get-cache (point)))
      (setq found-class (and cache (wisi-cache-class cache)))
      (unless (equal expected-class found-class)
	;; we don't abort here, so we can see all errors at once
	(setq error-p t)
	(message
	 (concat
	  (buffer-file-name) ":" (format "%d" (count-lines (point-min) (point))) ":\n"
	  (format "class does not match.\nGot    '%s',\nexpect '%s'"
		    found-class
		    expected-class)
	  ))))
    (when error-p
      (error
       (concat
	(buffer-file-name) ":" (format "%d" (line-number-at-pos))
	": aborting due to previous errors")))
    ))

(defun run-test (filename)
  (require (intern (concat filename "-wy")))
  ;; top level parse action must set `wisi-test-success' t.
  (let ((build-dir default-directory)
	(input-file (concat "../../test/wisi/" filename ".input")))
    (unless (file-readable-p input-file)
      (error "%s not found" input-file))
    (find-file input-file)
    (run-test-here filename)
    (setq default-directory build-dir)
    (with-temp-buffer
      (insert "success")
      (write-file (concat filename ".wisi-test")))
  ))

;; end of file
