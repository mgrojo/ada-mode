;; Emacs utilities for wisitoken

(defun wisitoken-ediff-good (good computed)
  "GOOD is the string name of the known good file (may be nil), COMPUTED the computed file"
  (cond
   ((and good computed)
    ;;  Names are accurate paths relative to current directory, with
    ;;  no line numbers.
    (ediff good computed)
    )

   ((and (null good) computed)
    ;; ediff COMPUTED against the corresponding _good file.
    ;;
    ;; COMPUTED includes the trailing line number, so we need to strip
    ;; it off.
    (let* ((filename-line computed)
	   (end (string-match ":[0-9]+$" filename-line))
	   (filename (if end (substring filename-line 0 end) filename-line))
	   (filename-good (concat filename "_good"))
	   (loc-filename (locate-file filename compilation-search-path))
	   (loc-filename-good (locate-file filename-good compilation-search-path)))
      (unless loc-filename
	(user-error "'%s' not found; wrong project?" filename))
      (unless loc-filename-good
	(user-error "'%s' not found; wrong project?" filename-good))
      (ediff loc-filename-good loc-filename)))
   ))

(defun wisitoken-update-good ()
  (interactive)
  ;; Point is after FAIL in a diff test fail message, because user
  ;; reviewed the single line of diff displayed and decided it was ok.
  ;;
  ;; Or point is on the next line, at the file name, because the user
  ;; just returned from viewing the ediff.
  ;;
  ;; FAIL wisi_wy_test.adb-empty_production_2 : Run_Test
  ;;     empty_production_2_lalr.parse_table:91
  ;;
  ;; replace the corresponding _good file with the new file.
  ;;
  ;; (thing-at-point ’filename) includes the trailing line number, so
  ;; we need to strip it off.

  (when (looking-back "FAIL" (line-beginning-position))
    (forward-line 1)
    (forward-word 1))

  (let* ((filename-line (thing-at-point 'filename))
     (end (string-match ":[0-9]+$" filename-line))
     (filename (if end (substring filename-line 0 end) filename-line))
     (filename-good (concat filename "_good")))
    (copy-file (locate-file filename compilation-search-path)
	       (locate-file filename-good compilation-search-path)
	       t)
    (message "%s updated" filename-good)))

(defun wisitoken-goto-aunit-fail ()
  ;; point is on aunit test file name (from `wisitoken-dtrt') in an AUnit test failure message:
  ;;
  ;; FAIL ^test_mckenzie_recover.adb : Empty_Comments
  ;;     1. 1.recover.ops. 1.id got  26 expecting  54
  ;;
  ;; goto that file and procedure
  (let ((case-fold-search nil)
	filename
	subprogram-name
	alg test-label error-number field
	(large-case-alg-present nil)
	(small-case-alg-present nil))
    (setq filename (thing-at-point 'filename))
    (end-of-line)
    (backward-word 1)
    (setq subprogram-name (thing-at-point 'symbol))
    (when (string-equal filename "test_mckenzie_recover.adb")
      (forward-symbol -2)
      (setq alg (thing-at-point 'symbol))
      (forward-line 1)
      (back-to-indentation)
      (when (looking-at "\\([0-9]+\\)?\\. \\([0-9]+\\)\\.\\([^ .']+\\)")
	(setq test-label (match-string 1))
	(setq error-number (match-string 2))
	(setq field (match-string 3)))
      ;; leave cursor on 'got' value
      (search-forward "got")
      (forward-char 2))
    (let* ((uniq-files (uniq-file-uniquify (project-files (project-current))))
	   (result (all-completions filename uniq-files))
	   (abs-file (cdr (assoc (car result) uniq-files #'string=)))
	   (display-buffer-overriding-action
	    (cons (list #'ofw-display-buffer-other-window) nil)))
      (when (not (stringp abs-file))
	(setq abs-file (car abs-file)))
      (find-file abs-file)
      (let ((case-fold-search nil)) ;; buffer-local
	(goto-char (point-min)) ;; for testing repeatedly :)
	(search-forward (concat "procedure " subprogram-name))
	(when (string-equal filename "test_mckenzie_recover.adb")
	  (let ((begin (point))
		end)
	    (save-excursion
	      (wisi-goto-statement-end)
	      (setq end (point)) ;; end of subprogram
	      (goto-char begin)
	      (when (search-forward "case Test.Alg" end t)
		(if (looking-back "(case Test.Alg")
		    (setq small-case-alg-present t)
		  (setq large-case-alg-present t))
		))

	    (when test-label (search-forward (concat "\"" test-label "\"")))

	    (cond
	     (large-case-alg-present
	      (search-forward (concat "when " alg))
	      (unless test-label (search-forward "Check_Recover"))
	      (when (and error-number
			 (not (string-equal error-number "1")))
		(search-forward-regexp (concat "Checking_Error +=> " error-number)))
	      (when field
		(search-forward field)
		(end-of-line)
		(forward-char -1)))

	     (small-case-alg-present
	      (if (or (null error-number)
		      (string-equal error-number "1"))
		  (unless test-label
		    (search-forward "Check_Recover"))
		(search-forward-regexp (concat "Checking_Error +=> " error-number)))
	      (when field
		(search-forward field)
		(if (search-forward "(case Test.Alg" (line-end-position) t)
		    (progn (search-forward (concat "when " alg))
			   (forward-word 1))
		  (end-of-line)
		  (forward-char -1))))

	     (t
	      (unless test-label (search-forward "Check_Recover"))
	      (when (and error-number
			 (not (string-equal error-number "1")))
		(search-forward-regexp (concat "Checking_Error +=> " error-number)))
	      (when field
		(search-forward field)
		(end-of-line)
		(forward-char -1)))
	     )))
	))))

(defun wisitoken-goto-stack-trace (file line column)
  ;; FILE LINE COLUMN is from an exception stack trace (from `wisitoken-dtrt'):
  ;;
  ;; wisitoken-syntax_trees.adb:1060
  ;;
  ;; goto it
  (find-file (locate-file file compilation-search-path))
  (goto-line (string-to-number line))
  (when column
    (move-to-column (string-to-number column))))

(defconst wisitoken-fail-re
  ;; no:
  ;; 0028: | | | (subprogram_body_0, (1 . 34), (1, 1) ERROR)
  ;; syntax_error: :1:1: in parse action error: ((MISSING_NAME_ERROR, ...
  ;; recover: fail FAIL_ENQUEUE_LIMIT, parser count 1
  ;;  2:  2, ( 1 0 0 0 0 0 0 0 0), MATCH_NAMES_ERROR (34 : ...

  ;;
  ;; yes (but only on first line?):
  ;; FAIL test_incremental.adb : Missing_Name_1
  ;;     parse_error: CONSTRAINT_ERROR: wisitoken-syntax_trees.ads:2780 discriminant check failed

  ;; exception: PROGRAM_ERROR: EXCEPTION_ACCESS_VIOLATION

  ;; ERROR test_mckenzie_recover.adb LALR : Error_4
  ;;   ADA.ASSERTIONS.ASSERTION_ERROR
  ;;   Exception Message: failed precondition from wisitoken-syntax_trees.ads:2227
  ;;
  ;;
  "^FAIL \\|^ERROR[: ]")

(defun wisitoken-compilation-prev ()
  (interactive)
  (forward-line -2)
  (let ((case-fold-search nil))
    (search-backward-regexp wisitoken-fail-re))
  )

(defun wisitoken-compilation-next ()
  (interactive)
  (let ((case-fold-search nil))
    (unless (search-forward-regexp wisitoken-fail-re nil t)
      (ding)
      (goto-char (point-min))
      (search-forward-regexp wisitoken-fail-re nil t)))
  )

(defun wisitoken-dtrt ()
  "Do The Right Thing"
  (interactive)
  ;; point is after FAIL or ERROR.

  ;; check for exception stack trace:
  ;;
  ;; task  1: SYSTEM.ASSERTIONS.ASSERT_FAIL^URE: failed precondition from wisitoken-syntax_trees.adb:1060
  ;; [C:\Projects\org.wisitoken.stephe-2\build\test_mckenzie_harness.exe]
  ;; Aunit.Test_Filters.Verbose.Is_Active at s-assert.adb:46
  ;; Aunit.Test_Filters.Verbose.Is_Active at wisitoken-syntax_trees.adb:1060
  ;;
  ;; task  1: CONSTRAINT_ERROR^: wisitoken-syntax_trees.adb:1572:27 access check failed
  ;; [C:\Projects\org.wisitoken.stephe-2\build\test_mckenzie_harness.exe]
  ;; Aunit.Test_Filters.Verbose.Is_Active at wisitoken-syntax_trees.adb:1572
  (cond
   ((search-forward-regexp ":.* \\([0-9a-z-_]+\\.[.a-z_]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?" (line-end-position) t)
    (wisitoken-goto-stack-trace (match-string 1) (match-string 2) (match-string 3)))

   (t
    ;; check for ediff fail:
    (forward-line)
    (back-to-indentation)

    (cond
     ((looking-at "[0-9a-z-_]+\\.[.a-z_]+:[0-9]+")
      ;; FAIL wisi_wy_test.adb-empty_production_2 : Run_Test
      ;;     ^empty_production_2_lalr.parse_table:91
      (wisitoken-ediff-good nil (match-string 0)))

     ((looking-at "\\([./a-z-_]+\\) longer than \\([./a-z-_]+\\)")
      ;; ^../test/bnf/body.parse_good longer than body.parse
      (wisitoken-ediff-good (match-string 1) (match-string 2)))

     (t
      (forward-line -1)
      (forward-word 1)
      (forward-char 1)
      (wisitoken-goto-aunit-fail))
     ))))

(define-key compilation-mode-map "d" #'wisitoken-dtrt)
(define-key compilation-mode-map "u" #'wisitoken-update-good)
(define-key compilation-mode-map "n" #'wisitoken-compilation-next)
(define-key compilation-mode-map "p" #'wisitoken-compilation-prev)

(defun wisitoken-fix-ops ()
  (interactive)
  ;; in test_mckenzie_recover.adb, point is on '(' in an Ops
  ;; list. Assume it was copied from compilation; convert one op to
  ;; required syntax.
  (forward-char 1)
  (let ((insertp (looking-at "INSERT"))
	(ffp (looking-at "FAST_FORWARD")))
    (forward-symbol 1)
    (let ((wisi-case-strict t))
      (wisi-case-adjust-at-point))
    (cond
     ((not ffp)
      (forward-char 2)
      (insert "+")
      (forward-symbol 1)
      (insert "_ID")
      (forward-symbol 1))

     (ffp
      (forward-char 2)
      (delete-char 1)
      (forward-symbol 1))
     )

    (forward-char 1); ')'
    (unless (eolp)
      (delete-char 1)
      (insert " &")
      (forward-char 1))
     ))

(require 'ada-mode)
(define-key ada-mode-map "\C-ca" #'wisitoken-fix-ops)

;; End of file
