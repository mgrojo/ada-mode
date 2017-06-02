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

(package-initialize)

(require 'cl-lib)
(require 'wisi)

;; Default includes mtn, among others, which is broken in Emacs 24.3
(setq vc-handled-backends '(CVS))

(defvar wisi-test-parser 'elisp
  "Set to ’process to test external process parser.")

(defvar test-syntax-table
  (let ((table (make-syntax-table)))
    ;; make-syntax-table sets all alphanumeric to w, etc; so we only
    ;; have to add test-specific things.

    ;; operator symbols
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?|  "." table)

    ;; \f and \n end a comment - see test-syntax-propertize for comment start
    (modify-syntax-entry ?\f  ">   " table)
    (modify-syntax-entry ?\n  ">   " table)
    table
    ))

(defconst test-class-list
  '(
    block-start
    close-paren
    function
    open-paren
    other
    procedure
    statement-end
    statement-other
    statement-start
    )
  )

(defun test-syntax-propertize (start end)
  "Assign `syntax-table' properties in accessible part of buffer."
  ;; (info "(elisp)Syntax Properties")
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t)
	(inhibit-read-only t)
	(inhibit-point-motion-hooks t)
	(inhibit-modification-hooks t))
    (goto-char start)
    (while (re-search-forward
	     "\\(--\\)"; 1: comment start
	    end t)
      ;; The help for syntax-propertize-extend-region-functions
      ;; implies that 'start end' will always include whole lines, in
      ;; which case we don't need
      ;; syntax-propertize-extend-region-functions
      (cond
       ((match-beginning 1)
	(put-text-property
	 (match-beginning 1) (match-end 1) 'syntax-table '(11 . nil)))
       ))
    (unless modified
      (restore-buffer-modified-p nil))))

(defun run-test-here (filename)
  ;; split out from run-test for interactive debugging
  (interactive "Mgrammar filename root: ")
  (let ((wisi-test-success nil)
	(expected-result t)
	(wisi--cache-max
	 (list
	  (cons 'face (copy-marker (point-min)))
	  (cons 'navigate (copy-marker (point-min)))
	  (cons 'indent (copy-marker (point-min)))))
	(wisi--parse-action 'navigate)) ;; for wisi-statement-action

    ;; use Ada style comments in source
    (set-syntax-table test-syntax-table)
    (set (make-local-variable 'syntax-propertize-function) 'test-syntax-propertize)
    (syntax-ppss-flush-cache (point-min));; force re-evaluate with hook.

    (cl-ecase wisi-test-parser
      (elisp
       (require 'wisi-elisp-parse)
       (require (intern (concat filename "-elisp")))
       (wisi-setup
	nil ;; indent-calculate
	nil ;; post-indent-fail
	test-class-list
	(wisi-make-elisp-parser
	 (symbol-value (intern-soft (concat filename "-elisp-parse-table")))
	 `wisi-forward-token)
	(wisi-make-elisp-lexer
	 :token-table (symbol-value (intern-soft (concat filename "-elisp-token-table")))
	 :keyword-table (symbol-value (intern-soft (concat filename "-elisp-keyword-table")))
	 :string-quote-escape-doubled nil
	 :string-quote-escape nil)))

      (process
       (require 'wisi-elisp-parse)
       (require (intern (concat filename "-process")))
       (add-to-list 'exec-path default-directory)
       (wisi-setup
	nil ;; indent-calculate
	nil ;; post-indent-fail
	test-class-list ;; class-list
	(wisi-make-process-parser
	 :label filename
	 :exec (concat filename "_wisi_parse.exe")
	 :token-table (nth 0 (symbol-value (intern-soft (concat filename "-process-token-table"))))
	 :action-table (nth 0 (symbol-value (intern-soft (concat filename "-process-action-table"))))
	 :terminal-hashtable (nth 1 (symbol-value (intern-soft (concat filename "-process-token-table")))))
	(wisi-make-elisp-lexer
	 :token-table (symbol-value (intern-soft (concat filename "-elisp-token-table")))
	 :keyword-table (symbol-value (intern-soft (concat filename "-elisp-keyword-table")))
	 :string-quote-escape-doubled nil
	 :string-quote-escape nil)))
      )

    ;; Not clear why this is not being done automatically
    (syntax-propertize (point-max))

    ;; Check for expected error result
    (goto-char (point-min))
    (when (re-search-forward "--PARSE_RESULT:" nil t)
      (setq expected-result (eval (buffer-substring-no-properties (point) (line-end-position)))))

    (goto-char (point-min))
    (condition-case-unless-debug err
      (wisi-parse-current wisi--parser)
      (error
       (setq wisi-test-success
	     (equal (cdr err) expected-result))
       (unless wisi-test-success
	 (message err))))
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
  (add-to-list 'load-path "../../test/wisi/")

  ;; top level parse action must set `wisi-test-success' t.

  ;; fail for any parse errors.
  ;;
  ;; IMPROVEME: report failure in .wisi-test file, so 'make' can run all
  ;; tests and report all failures.
  (setq wisi-debug 1)
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

(provide 'run-wisi-test)
;; end of file
