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
(require 'wisi-parse)
(require 'wisi)

;; Default includes mtn, among others, which is broken in Emacs 24.3
(setq vc-handled-backends '(CVS))

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
  (let ((parse-table (symbol-value (intern-soft (concat filename "-elisp-parse-table"))))
	(wisi-test-success nil)
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

  (cl-ecase wisi-parser
    (elisp
     (wisi-setup
      nil ;; indent-calculate
      nil ;; post-indent-fail
      test-class-list
      (symbol-value (intern-soft (concat filename "-elisp-keyword-table")))
      (symbol-value (intern-soft (concat filename "-elisp-token-table")))
      parse-table
      nil nil))

    (ada
     (setq wisi-ext-parse-exec (concat filename "_wisi_parse.exe"))
     (add-to-list 'exec-path default-directory)
     (wisi-setup
      nil ;; indent-calculate
      nil ;; post-indent-fail
      test-class-list ;; class-list
      nil ;; keyword-table
      nil ;; token-table
      nil ;; parse-table
     (symbol-value (intern-soft (concat filename "-process-token-table")))
     (car (symbol-value (intern-soft (concat filename "-process-action-table"))))))
    )

    ;; Not clear why this is not being done automatically
    (syntax-propertize (point-max))

    ;; Check for expected error result
    (goto-char (point-min))
    (when (re-search-forward "--PARSE_RESULT:" nil t)
      (setq expected-result (eval (buffer-substring-no-properties (point) (line-end-position)))))

    (goto-char (point-min))
    (condition-case-unless-debug err
	(cl-ecase wisi-parser
	  (elisp
	   (wisi-parse parse-table 'wisi-forward-token))
	  (ada
	   (wisi-ext-parse wisi-token-names wisi-action-names))
	  )
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

  (cl-ecase wisi-parser
    (elisp
     (require (intern (concat filename "-elisp"))))
    (ada
     (require (intern (concat filename "-process"))))

    )

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
