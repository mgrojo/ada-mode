;; Ada specific utils for automating indentation and casing tests

(require 'wisi-run-indent-test)

(defun switch-to-lr1 ()
  (setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lr1_parse.exe" ada-mode-dir))
  (setq wisi-process--alist nil)
  (ada-mode))

(defun test-moom (search-string refactor-string)
  "Refactor method (object ...) to object.method (...)"
  (test-refactor-1 ada-refactor-method-object-to-object-method
		   ada-refactor-object-method-to-method-object
		   search-string refactor-string))

(defun test-oieo (search-string refactor-string)
  (test-refactor-1 ada-refactor-object-index-to-element-object
		   ada-refactor-element-object-to-object-index
		   search-string refactor-string))

(defun test-all-refs (name)
  "Return list of (FILENAME CATEGORY) for all references of NAME."
  (unless (bolp)
    ;; We are at bol if we did (forward-line -n). Otherwise, skip the (test-all-defs ...).
    (end-of-line))
  (search-forward name)
  (backward-word 1)
  (let* ((prj (project-current))
	 (xrefs (xref-backend-references prj (xref-backend-identifier-at-point (xref-find-backend))))
	 result)
    (dolist (ref xrefs)
      (with-slots (summary location) ref
	(with-slots (file) location
	  (push (list (file-name-nondirectory file) summary) result))))
    result))

(defun test-all-defs (name)
  "Return list of (FILENAME CATEGORY) for all definitions of NAME (and parent and child types)."
  (unless (bolp)
    ;; We are at bol if we did (forward-line -n). Otherwise, skip the (test-all-defs ...).
    (end-of-line))
  (search-forward name)
  (backward-word 1)
  (let* ((prj (project-current))
	 (xrefs (xref-backend-definitions prj (xref-backend-identifier-at-point (xref-find-backend))))
	 result)
    (dolist (ref xrefs)
      (with-slots (summary location) ref
	(with-slots (file) location
	  (push (list (file-name-nondirectory file) summary) result))))
    result))

(provide 'run-indent-test)
;; end of file
