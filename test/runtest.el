;; utils for automating ada-mode indentation tests

;; Default includes mtn, among others, which is broken in Emacs 22.2
(setq vc-handled-backends '(CVS))

(require 'ada-mode)
(require 'ada-indent)

(defun run-test-here ()
  "Run an ada-mode test on the current buffer."
  (interactive)
  (let (last-result last-cmd)
    ;; Look for --EMACS comments in the file:
    ;;
    ;; --EMACSCMD: <form>
    ;;    Executes the lisp form. This is mostly used to set non-default values
    ;;    for ada-mode options; it can also be used to test other things.
    ;;
    ;; --EMACSRESULT: <form>
    ;;    <form> is evaluated and compared (using `equal') with the
    ;;    result of the previous EMACSCMD, and the test fails if they
    ;;    don't match.

    (goto-char (point-min))
    (while (re-search-forward "--EMACS\\(CMD\\|RESULT\\):" nil t)
      (cond
       ((string= (match-string 1) "CMD")
        (looking-at ".*$")
        (save-excursion
          (setq last-cmd (match-string 0)
                last-result (eval (car (read-from-string last-cmd))))))

       ((string= (match-string 1) "RESULT")
        (looking-at ".*$")
        (unless (equal (car (read-from-string (match-string 0))) last-result)
          (message (concat "Result of "
                           (with-output-to-string (princ last-cmd))
                           " does not match. Got --"
                           (with-output-to-string (princ last-result))
                           "--, expect --" (match-string 0)
                           "--"))
          (error "Result does not match")))

       (t
        (error (concat "Unexpected command " (match-string 1))))))
    )

  ;; Reindent and recase the buffer
  (setq ada-clean-buffer-before-saving nil)

  ;; first unindent; if the indentation rules do nothing, the test would pass, otherwise!
  (setq indent-tabs-mode nil)
  (indent-code-rigidly (point-min) (point-max) -4)
  (indent-region (point-min) (point-max))
  ;; FIXME: put this back (and fix it!) lowercase-buffer? (ada-adjust-case-buffer)
  ;; FIXME: also test case-exceptions (ie GDS, Text_IO)

  ;; Cleanup the buffer; indenting often leaves trailing whitespace;
  ;; files must be saved without any.
  (delete-trailing-whitespace)
  )

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

  (find-file file-name)
  ;; we don't do (ada-mode) here; that should be done by the file name
  ;; extension, and some file local variables may assume ada-mode is
  ;; already active, and change things that this would then wipe
  ;; out. If it's not done by the extension, add a file local variable
  ;; to set ada-mode.
  (run-test-here)

  ;; Write the result file; makefile will diff.
  ;;
  ;; FIXME: if run-test-here fails, generate some sort of error so we
  ;; see it when running from the Makefile.
  (write-file (concat file-name ".tmp"))
  )
;; Local Variables:
;; eval: (add-to-list 'load-path (expand-file-name "../"))
;; End:
;; end of file
