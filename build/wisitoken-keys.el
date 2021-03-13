;; Emacs utilities for wisitoken

(defun wisitoken-gnat-fix-error (msg source-buffer source-window)
  "For `ada-gnat-fix-error-hook'."

  (let ((start-pos (point))
	result)
    ;; Move to start of error message text
    (skip-syntax-forward "^-")
    (forward-char 1)

    ;; recognize it, handle it
    (setq
     result
     (unwind-protect
	 (cond
	  ;; Wisitoken access type naming convention
	  ((looking-at (concat "expected \\(private \\)?type " ada-gnat-quoted-name-regexp))
	   (let ((type (match-string 2)))
	     (next-line 1)
	     (when (looking-at "found type .*_Ptr")
	       ;; assume just need '.all'
	       (progn
		 (pop-to-buffer source-buffer)
		 (forward-word 1)
		 (insert ".all")
		 t)
	       )))
	  ;; Ada hidden in wisi packages
	  ((looking-at "package \"Ada\" is hidden by declaration")
	   (pop-to-buffer source-buffer)
	   (backward-word 1)
	   (insert "Standard.")
	   t)

	  )));; end of setq unwind-protect cond
    (if result
	t
      (goto-char start-pos)
      nil)
    ))

(add-hook 'ada-gnat-fix-error-hook 'wisitoken-gnat-fix-error)

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

(defconst wisitoken-fail-re "FAIL\\|ERROR")

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
  "Either ediff or goto-aunit-fail"
  (interactive)
  ;; point is after FAIL. Distinguish between aunit and ediff:
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
   ))

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
