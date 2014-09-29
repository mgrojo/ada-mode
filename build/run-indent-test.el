;; utils for automating indentation and casing tests

;; Default includes mtn, among others, which is broken in Emacs 22.2
(setq vc-handled-backends '(CVS))

;; user can set these to t in an EMACSCMD
(defvar skip-cmds nil)
(defvar skip-reindent-test nil)
(defvar skip-recase-test nil)

(defun test-face (token face)
  "Test if TOKEN in next code line has FACE.
FACE may be a list; emacs 24.3.93 uses nil instead of 'default."
  (save-excursion
    (when (ada-in-comment-p)
      (beginning-of-line); forward-comment doesn't move if inside a comment!
      (forward-comment (point-max)))
    (condition-case err
	(search-forward token (line-end-position))
      (error
       (error "can't find '%s'" token)))
    (goto-char (match-beginning 0))
    (unless (or (and (listp face)
		     (memq (face-at-point) face))
		(eq (face-at-point) face))

      (error
       "found face %s, expecting %s for '%s'"
	(face-at-point)
	face
	token))
    ))

(defun run-test-here ()
  "Run an indentation and casing test on the current buffer."
  (interactive)
  (setq indent-tabs-mode nil)
  (setq debug-on-quit t)
  (let (last-result last-cmd expected-result error-p)
    ;; Look for --EMACS comments in the file:
    ;;
    ;; --EMACSCMD: <form>
    ;;    Executes the lisp form inside a save-excursion, saves the result as a lisp object.
    ;;
    ;; --EMACSRESULT: <form>
    ;;    point is moved to end of line, <form> is evaluated inside
    ;;    save-excursion and compared (using `equal') with the result
    ;;    of the previous EMACSCMD, and the test fails if they don't
    ;;    match.
    ;;
    ;; --EMACSDEBUG: <form>
    ;;    Eval form, display result. Also used for setting breakpoint.

    (goto-char (point-min))
    (while (and (not skip-cmds)
		(re-search-forward "--EMACS\\(CMD\\|RESULT\\|DEBUG\\):" nil t))
      (cond
       ((string= (match-string 1) "CMD")
	(looking-at ".*$")
	(save-excursion
	  (setq last-cmd (match-string 0)
		last-result
		(if debug-on-error
		    ;; let debug-on-error work
		    (eval (car (read-from-string last-cmd)))

		  ;; not debug
		  (condition-case err
		      (eval (car (read-from-string last-cmd)))
		    (error
		     (setq error-p t)
		     (message
		      (concat
		       (buffer-file-name) ":" (format "%d" (line-number-at-pos))
		       ": command: %s") last-cmd)
		     (message
		      (concat
		       (buffer-file-name) ":" (format "%d" (count-lines (point-min) (point)))
		       ": %s: %s") (car err) (cdr err)))))
		)))

       ((string= (match-string 1) "RESULT")
	(looking-at ".*$")
	(setq expected-result (save-excursion (end-of-line 1) (eval (car (read-from-string (match-string 0))))))
	(unless (equal expected-result last-result)
	  ;; we don't abort here, so we can see all errors at once
	  (setq error-p t)
	  (message
	   (concat
	    (format "error: %s:%d:\n" (buffer-file-name) (line-number-at-pos))
	    (format "Result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
		    last-cmd
		    last-result
		    expected-result)
	    ))))

       ((string= (match-string 1) "DEBUG")
	(looking-at ".*$")
	(message "DEBUG: %s:%d %s"
		 (current-buffer)
		 (line-number-at-pos)
		 (eval (car (read-from-string (match-string 0))))))

       (t
	(setq error-p t)
	(error (concat "Unexpected command " (match-string 1))))))

    (when error-p
      (error
       (concat
	    (buffer-file-name) ":" (format "%d" (count-lines (point-min) (point)))
	    ": aborting due to previous errors")))
    )

  (when (not skip-reindent-test)
    ;; Reindent the buffer
    (message "indenting")

    ;; first unindent; if the indentation rules do nothing, the test
    ;; would pass, otherwise!  Only unindent by 1 column, so comments
    ;; not currently in column 0 are still not in column 0, in case
    ;; the mode supports a special case for comments in column 0.
    (indent-rigidly (point-min) (point-max) -1)

    ;; indent-region uses save-excursion, so we can't goto an error location
    (indent-region (point-min) (point-max))

    ;; Cleanup the buffer; indenting often leaves trailing whitespace;
    ;; files must be saved without any.
    (delete-trailing-whitespace)
    )

  (when (and (not skip-recase-test)
	     (eq major-mode 'ada-mode))
    ;; gpr-mode doesn't support casing yet
    (message "casing")
    (ada-case-adjust-buffer))
  )

(defun run-test (file-name)
  "Run an indentation and casing test on FILE-NAME."
  (interactive "f")
  ;; we'd like to run emacs from a makefile as:
  ;;
  ;; emacs -Q --batch -l runtest.el -f run-test-here <filename>
  ;;
  ;; However, the function specified with -f is run _before_
  ;; <filename> is visited. So we try this instead:
  ;;
  ;; emacs -Q --batch -l runtest.el --eval '(run-test "<filename>")'
  ;;
  ;; And then we discover that processes spawned with start-process
  ;; don't run when emacs is in --batch mode. So we try this:
  ;;
  ;; emacs -Q -l runtest.el --eval '(progn (run-test "<filename>")(kill-emacs))'
  ;;
  ;; Then we have problems with font lock defaulting to jit-lock; that
  ;; screws up font-lock tests because the test runs before jit-lock
  ;; does. This forces default font-lock, which fontifies the whole
  ;; buffer when (font-lock-fontify-buffer) is called, which tests
  ;; that rely on font-lock do explicitly.
  (setq font-lock-support-mode nil)

  (let ((dir default-directory))
    (find-file file-name)

    (run-test-here)

    ;; Write the result file; makefile will diff.
    (when skip-reindent-test
      ;; user sets skip-reindent-test when testing interactive editing
      ;; commands, so the diff would fail. Revert to the original file,
      ;; save a copy of that.
      (revert-buffer t t))

    (write-file (concat dir (file-name-nondirectory file-name) ".tmp"))
    )
  )

(provide 'run-indent-test)

;; Local Variables:
;; eval: (add-to-list 'load-path (expand-file-name "../"))
;; End:
;; end of file
