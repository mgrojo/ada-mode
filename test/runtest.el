;; utils for automating ada-mode indentation tests

;; Default includes mtn, among others, which is broken in Emacs 22.2
(setq vc-handled-backends '(CVS))

(require 'ada-mode)
(require 'ada-smie-opentoken)

(defvar skip-reindent-test nil);; user can set to t in an EMACSCMD
(defvar skip-cmds nil);; user can set to t in an EMACSCMD

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
  "Run an ada-mode test on the current buffer."
  (interactive)
  (setq indent-tabs-mode nil)
  (let (last-result last-cmd expected-result)
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
		last-result (eval (car (read-from-string last-cmd))))))

       ((string= (match-string 1) "RESULT")
	(looking-at ".*$")
	(setq expected-result (save-excursion (eval (car (read-from-string (match-string 0))))))
	(unless (equal expected-result last-result)
	  (error
	   (concat
	    (buffer-file-name) ":" (format "%d" (count-lines (point-min) (point))) ":\n"
	    (format "Result of '%s' does not match.\nGot    '%s',\nexpect '%s'"
		    last-cmd
		    last-result
		    expected-result)
	    ))))

       (t
	(error (concat "Unexpected command " (match-string 1))))))
    )

  (when (not skip-reindent-test)
    ;; Reindent and recase the buffer
    (setq ada-clean-buffer-before-saving nil)

    ;; first unindent; if the indentation rules do nothing, the test
    ;; would pass, otherwise!  Only unindent by 1 column, so comments
    ;; not currently in column 0 are still not in column 0, in case
    ;; ada-indent-comment-column-0 is t
    (indent-code-rigidly (point-min) (point-max) -1)
    (indent-region (point-min) (point-max))
    (ada-case-adjust-buffer)

    ;; Cleanup the buffer; indenting often leaves trailing whitespace;
    ;; files must be saved without any.
    (delete-trailing-whitespace)
    ))

(defun run-test (file-name)
  "Run an ada-mode test on FILE-NAME."
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

    ;; we don't do (ada-mode) here; that should be done by the file
    ;; name extension, and some file local variables may assume
    ;; ada-mode is already active, and change things that this would
    ;; then wipe out. If it's not done by the extension, add a file
    ;; local variable to set ada-mode.
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
;; Local Variables:
;; eval: (add-to-list 'load-path (expand-file-name "../"))
;; End:
;; end of file
