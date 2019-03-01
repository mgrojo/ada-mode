;; Project definitions

(require 'ada-project)
(require 'project-menu)
(require 'xref-ada)

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file (expand-file-name "wisitoken.prj"))
       (prj-name "wisitoken main")
       (prj (make-ada-project
	     :env-vars nil
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )

;; extensions to ada-fix-error

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
  ;; point is on a file in a test fail message:
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

(define-key compilation-mode-map "e" 'wisitoken-ediff-good)
;; end of file
