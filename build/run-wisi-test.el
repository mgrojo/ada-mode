;; Run as:
;;
;; $(EMACS) -Q -batch -l run-wisi-test.el --eval (run-test "<filename>")
;;
;; to debug a test:
;; (add-to-list 'load-path "/Projects/org.emacs.ada-mode")
;; (add-to-list 'load-path "/Projects/org.emacs.ada-mode/build")
;; (load "run-wisi-test")
;; from Makefile:
;; M-x : (run-test "<filename>" t)

(require 'cl-lib)
(require 'wisi)
(require 'wisi-tests)

(defun run-test-here (filename)
  ;; split out from run-test for interactive debugging
  (interactive "Mgrammar filename root: ")
  (let ((wisi-test-success nil)
	(expected-result t)
	(wisi--parse-action 'navigate)) ;; for wisi-statement-action

    (wisi-tests-setup filename)

    ;; Check for expected error result
    (goto-char (point-min))
    (when (re-search-forward "--PARSE_RESULT:" nil t)
      (setq expected-result (eval (buffer-substring-no-properties (point) (line-end-position)))))

    (goto-char (point-min))
    (condition-case-unless-debug err
      (wisi-parse-current wisi--parser (point-min) (point-max) (point-max))
      (wisi-parse-error
       (setq wisi-test-success
	     (equal (cdr err) expected-result))
       (unless wisi-test-success
	 (message (cdr err)))))
      ;; parse action must set wisi-test-success t
    (unless wisi-test-success
      (error "parse test failed")))

  ;; check action results; --CACHE: <form> compares cached class on next token to <form>
  (let (expected-class found-class cache error-p)
    (goto-char (point-min))
    (while (re-search-forward "--CACHE:" nil t)
      (setq expected-class
	    (eval (car (read-from-string (buffer-substring-no-properties (point) (line-end-position))))))
      (forward-line 1)
      (setq cache (wisi-get-cache (point)))
      (setq found-class (and cache (wisi-cache-class cache)))
      (unless (equal expected-class found-class)
	;; we don't abort here, so we can see all errors at once
	(setq error-p t)
	(message
	 (concat
	  (buffer-file-name) ":" (format "%d" (line-number-at-pos)) ":\n"
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
  (interactive "Mgrammar filename root: ")
  (add-to-list 'load-path (expand-file-name "../test/wisi/"))

  ;; top level parse action must set `wisi-test-success' t.

  ;; fail for any parse errors.
  ;;
  ;; IMPROVEME: report failure in .wisi-test file, so 'make' can run all
  ;; tests and report all failures.
  (setq wisi-debug 1)
  (let ((build-dir default-directory)
	(input-file (concat "../test/wisi/" filename ".input")))
    (unless (file-readable-p input-file)
      (error "%s not found" input-file))
    (find-file input-file)
    (run-test-here filename)
    (setq default-directory build-dir)
    (with-temp-buffer
      (insert "success")
      (write-file (concat filename ".wisi-test")))
  ))

(provide 'run-wisi-test)
;; end of file
