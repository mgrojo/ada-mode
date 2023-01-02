;; Ada specific utils for automating indentation and casing tests

(package-initialize) ;; for uniquify-files, installed wisi, ada-mode

(require 'wisi-run-indent-test)

;; Let edebug display strings full-length, and show internals of records
;; this is also done in run-test; we do it here for 'make one-debug'

(defun switch-to-lr1 ()
  (setq ada-process-parse-exec (expand-file-name "bin/ada_mode_wisi_lr1_parse" ada-mode-dir))
  (setq wisi-process--alist nil)
  (ada-mode)
  (wisi-wait-parser))

(defun test-moom (search-string refactor-string)
  "Refactor method (object ...) to object.method (...)"
  (test-refactor-1 ada-refactor-method-object-to-object-method
		   ada-refactor-object-method-to-method-object
		   search-string refactor-string))

(defun test-oieo (search-string refactor-string)
  (test-refactor-1 ada-refactor-object-index-to-element-object
		   ada-refactor-element-object-to-object-index
		   search-string refactor-string))

(defun test-xref-helper (name generator)
  "Call GENERATOR with point on NAME, return list of (FILENAME CATEGORY) from resulting xrefs"
  (unless (bolp)
    ;; We are at bol if we did (forward-line -n). Otherwise, skip the (test-* "NAME").
    (end-of-line))
  (search-forward-regexp name)
  (backward-word 1)
  (let ((xrefs (funcall generator))
	result)
    (dolist (ref xrefs)
      (push (list (file-name-nondirectory
		   (if (functionp 'xref-file-location-file)
		       (xref-file-location-file (xref-item-location ref))
		     (oref (xref-item-location ref) file)))
		  (xref-item-summary ref))
		result))
    (nreverse result)))

(defun test-all-refs (name)
  "Return list of (FILENAME CATEGORY) for all (class-wide) references of NAME."
  (let ((prj (project-current)))
    (test-xref-helper
     name
     (lambda () (xref-backend-references
		 (xref-find-backend)
		 (if (eq ada-xref-backend 'eglot)
		     ;; eglot doesn't actually implement xref-backend-identifier-at-point
		     (thing-at-point 'symbol)
		   (xref-backend-identifier-at-point (xref-find-backend))))))
    ))

(defun test-all-defs (name &optional no-line-col)
  "Return list of (FILENAME CATEGORY) for all definitions of NAME (and parent and child types)."
  (let ((prj (project-current)))
    (test-xref-helper
     name
     (lambda () (xref-backend-definitions
		 (xref-find-backend)
		 (if (or no-line-col (eq ada-xref-backend 'eglot))
		     ;; eglot doesn't actually implement xref-backend-identifier-at-point
		     (thing-at-point 'symbol)
		   (xref-backend-identifier-at-point (xref-find-backend))))))
    ))

;;; Settings for all tests; can be overridden on make command line via
;;; ELISP, or in file via EMACSCMD or Local Variables.
(setq debug-on-error nil)
(setq eval-expression-debug-on-error nil)
(setq-default wisi-parser-verbosity "debug=1")
(setq-default compare-tree-text t)
(setq-default wisi-process-time-out 30.0) ;; running with debug/assert is slow, ada_mode-recover_30.adb

(setq wisi-debug 1) ;; abort on non-syntax errors

(provide 'run-indent-test)
;; end of file
