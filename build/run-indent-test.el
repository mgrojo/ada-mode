;; utils for automating indentation and casing tests

;; Default includes mtn, among others, which is broken in Emacs 22.2
(setq vc-handled-backends '(CVS))

;; user can set these to t in an EMACSCMD
(defvar skip-cmds nil)
(defvar skip-reindent-test nil)
(defvar skip-recase-test nil)

(defun test-face (token face)
  "Test if TOKEN in next code line has FACE."
  (save-excursion
    (beginning-of-line); forward-comment doesn't move if inside a comment!
    (forward-comment (point-max))
    (condition-case err
	(search-forward token (line-end-position))
      (error
       (error
	"%s:%d: can't find '%s'"
	(buffer-file-name)
	(count-lines (point-min) (point))
	token)))
    (goto-char (match-beginning 0))
    (unless (eq (face-at-point) face)
      (error
       "%s:%d: found face %s, expecting %s for '%s'"
	(buffer-file-name)
	(count-lines (point-min) (point))
	(face-at-point)
	face
	token))
    ))

(defun run-test-here ()
  "Run an indentation and casing test on the current buffer."
  (interactive)
  (setq indent-tabs-mode nil)
  (let (last-result last-cmd expected-result error-p)
    ;; Look for --EMACS comments in the file:
    ;;
    ;; --EMACSCMD: <form>
    ;;    Executes the lisp form inside a save-excursion, saves the result as a lisp object.
    ;;
    ;; --EMACSRESULT: <form>
    ;;    <form> is evaluated inside save-excursion and compared
    ;;    (using `equal') with the result of the previous EMACSCMD,
    ;;    and the test fails if they don't match.

    (goto-char (point-min))
    (while (and (not skip-cmds)
		(re-search-forward "--EMACS\\(CMD\\|RESULT\\):" nil t))
      (cond
       ((string= (match-string 1) "CMD")
	(looking-at ".*$")
	(save-excursion
	  (setq last-cmd (match-string 0)
		last-result
		(condition-case err
		    (eval (car (read-from-string last-cmd)))
		  (error
		   (setq error-p t)
		   (message
		    (concat
		     (buffer-file-name) ":" (format "%d" (count-lines (point-min) (point)))
		     ": %s: %s") (car err) (cdr err)))))))

       ((string= (match-string 1) "RESULT")
	(looking-at ".*$")
	(setq expected-result (save-excursion (eval (car (read-from-string (match-string 0))))))
	(unless (equal expected-result last-result)
	  ;; we don't abort here, so we can see all errors at once
	  (setq error-p t)
	  (message
	   (concat
	    (buffer-file-name) ":" (format "%d" (count-lines (point-min) (point))) ":\n"
	    (format "Result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
		    last-cmd
		    last-result
		    expected-result)
	    ))))

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

    ;; first unindent; if the indentation rules do nothing, the test
    ;; would pass, otherwise!  Only unindent by 1 column, so comments
    ;; not currently in column 0 are still not in column 0, in case
    ;; the mode supports a special case for comments in column 0.
    (indent-code-rigidly (point-min) (point-max) -1)

    ;; indent-region uses save-excursion, so we can't goto an error location
    (indent-region (point-min) (point-max))

    ;; Cleanup the buffer; indenting often leaves trailing whitespace;
    ;; files must be saved without any.
    (delete-trailing-whitespace)
    )

  (when (and (not skip-recase-test)
	     (eq major-mode 'ada-mode))
    ;; gpr-mode doesn't support casing yet
    (ada-case-adjust-buffer))
  )

(defun run-test (file-name)
  "Run an indentation and casing test on FILE-NAME."
  (interactive "f")
  ;; we'd like to run emacs from a makefile as:
  ;;
  ;; emacs --batch -l runtest.el -f run-test-here <filename>
  ;;
  ;; However, the function specified with -f is run _before_
  ;; <filename> is visited. So we do this instead:
  ;;
  ;; emacs --batch -l runtest.el --eval (run-test "<filename>")

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
