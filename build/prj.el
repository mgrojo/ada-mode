;; Project definitions

(wisi-prj-select-cache
 "wisitoken.prj"
 (create-ada-prj
  :name "wisitoken main"
  :compile-env
  '("SAL=../../org.stephe_leake.sal"))
  "Makefile")

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

(defun wisitoken-ediff-good ()
  (interactive)
  ;; point is on a file in a diff test fail message:
  ;;
  ;; FAIL wisi_wy_test.adb-empty_production_2 : Run_Test
  ;;     empty_production_2_lalr.parse_table:91
  ;;
  ;; ediff that file against the corresponding _good file.
  ;;
  ;; (thing-at-point ’filename) includes the trailing line number, so
  ;; we need to strip it off.

  (let* ((filename-line (thing-at-point 'filename))
     (end (string-match ":[0-9]+$" filename-line))
     (filename (if end (substring filename-line 0 end) filename-line))
     (filename-good (concat filename "_good")))
    (ediff (locate-file filename-good compilation-search-path)
       (locate-file filename compilation-search-path))))

(defun wisitoken-update-good ()
  (interactive)
  ;; point is on a file in a diff test fail message:
  ;;
  ;; FAIL wisi_wy_test.adb-empty_production_2 : Run_Test
  ;;     empty_production_2_lalr.parse_table:91
  ;;
  ;; replace the corresponding _good file with the new file.
  ;;
  ;; (thing-at-point ’filename) includes the trailing line number, so
  ;; we need to strip it off.

  (let* ((filename-line (thing-at-point 'filename))
     (end (string-match ":[0-9]+$" filename-line))
     (filename (if end (substring filename-line 0 end) filename-line))
     (filename-good (concat filename "_good")))
    (copy-file (locate-file filename compilation-search-path)
	       (locate-file filename-good compilation-search-path)
	       t)
    (message "%s updated" filename-good)))

(defun wisitoken-goto-aunit-fail ()
  (interactive)
  ;; point is on the first line in an AUnit test failure message:
  ;;
  ;; FAIL test_mckenzie_recover.adb : Empty_Comments
  ;;     1. 1.recover.ops. 1.id got  26 expecting  54
  ;;
  ;; goto that file and procedure
  (let (filename
	subprogram-name
	alg test-label error-number field
	(large-case-alg-present nil)
	(small-case-alg-present nil))
    (beginning-of-line)
    (forward-word 2)
    (setq filename (thing-at-point 'filename))
    (end-of-line)
    (backward-word 1)
    (setq subprogram-name (thing-at-point 'symbol))
    (when (string-equal filename "test_mckenzie_recover.adb")
      (forward-symbol -2)
      (setq alg (thing-at-point 'symbol))
      (forward-line 1)
      (back-to-indentation)
      (looking-at "\\([0-9]+\\)?\\. \\([0-9]+\\)\\.\\([^ .']+\\)")
      (setq test-label (match-string 1))
      (setq error-number (match-string 2))
      (setq field (match-string 3))
      ;; leave cursor on 'got' value
      (search-forward "got")
      (forward-char 2))
    (let ((abs-file (project-expand-file-name (project-current) filename))
	  (display-buffer-overriding-action
	   (cons (list #'ofw-display-buffer-other-window) nil)))
      (when (not (stringp abs-file))
	(setq abs-file (car abs-file)))
      (find-file abs-file)
      (xref-find-definitions (xref-expand-identifier (xref-find-backend) subprogram-name))
      (when (string-equal filename "test_mckenzie_recover.adb")
	(let ((begin (point))
	      end)
	  (save-excursion
	    (wisi-goto-statement-end)
	    (setq end (point))
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
	    (when (not (string-equal error-number "1"))
	      (search-forward-regexp (concat "Checking_Error +=> " error-number)))
	    (search-forward field)
	    (end-of-line)
	    (forward-char -1))

	   (small-case-alg-present
	    (if (string-equal error-number "1")
		(unless test-label
		  (search-forward "Check_Recover"))
	      (search-forward-regexp (concat "Checking_Error +=> " error-number)))
	    (search-forward field)
	    (if (search-forward "(case Test.Alg" (line-end-position) t)
		(progn (search-forward (concat "when " alg))
		       (forward-word 3))
	      (end-of-line)
	      (forward-char -1)))

	   (t
	    (unless test-label (search-forward "Check_Recover"))
	    (when (not (string-equal error-number "1"))
	      (search-forward-regexp (concat "Checking_Error +=> " error-number)))
	    (search-forward field)
	    (end-of-line)
	    (forward-char -1))
	   )))
      )))

(defun wisitoken-compilation-finish ()
  (forward-line)
  (back-to-indentation)
  (unless (looking-at "[0-9a-z-_]+\\.[a-z_]+:[0-9]+") ;; a file diff failed
    ;; an AUnit test failed
    (forward-line -1)
    (forward-word 1)
    (forward-char 1)
  ))

(defun wisitoken-compilation-prev ()
  (interactive)
  (forward-line -2)
  (let ((case-fold-search nil))
    (search-backward "FAIL"))
  (wisitoken-compilation-finish)
  )

(defun wisitoken-compilation-next ()
  (interactive)
  (let ((case-fold-search nil))
    (search-forward "FAIL"))
  (wisitoken-compilation-finish)
  )

(define-key compilation-mode-map "e" #'wisitoken-ediff-good)
(define-key compilation-mode-map "u" #'wisitoken-update-good)
(define-key compilation-mode-map "n" #'wisitoken-compilation-next)
(define-key compilation-mode-map "p" #'wisitoken-compilation-prev)
(define-key compilation-mode-map "g" #'wisitoken-goto-aunit-fail)

(defun wisitoken-fix-ops ()
  (interactive)
  ;; in test_mckenzie_recover.adb, point is on '(' in an Ops
  ;; list. Assume it was copied from compilation; convert one op to
  ;; required syntax.
  (forward-char 1)
  (let ((insertp (looking-at "INSERT"))
	(ffp (looking-at "FAST_FORWARD")))
    (forward-word 1)
    (ada-case-adjust-at-point)
    (cond
     ((not ffp)
      (forward-char 2)
      (insert "+")
      (forward-word)
      (insert "_ID")
      (forward-word 1)
      (when (= ?, (char-after))
	;; extra insert state
	(delete-char 1)
	(kill-word 2))
      (when insertp
	(insert ", 1, 0"))
      (forward-char 1); ')'
      (delete-char 1)
      (insert " &")
      (forward-char 1))

     (ffp
      (forward-char 2)
      (delete-char 1)
      (forward-word)
      (forward-char 1); ')'
      (delete-char 1)
      (insert " &")
      (forward-char 1))
     )))

(define-key ada-mode-map "\C-ca" #'wisitoken-fix-ops)


;; End of file
