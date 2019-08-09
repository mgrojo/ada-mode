;; Ada specific utils for automating indentation and casing tests

(require 'wisi-run-indent-test)

(defun switch-to-lr1 ()
  (setq ada-process-parse-exec (expand-file-name "ada_mode_wisi_lr1_parse.exe" ada-mode-dir))
  (setq wisi-process--alist nil)
  (ada-mode))

(defun ada-test-casing ()
  "Run a casing test on the current buffer."
  (interactive)
  (unless (or skip-recase-test
	      (eq major-mode 'gpr-mode))
    (message "casing")
    (ada-case-adjust-buffer)))

(defun test-moom (search-string refactor-string)
  "Refactor method (object ...) to object.method (...)"
  (test-refactor-1 ada-refactor-method-object-to-object-method
		   ada-refactor-object-method-to-method-object
		   search-string refactor-string))

(defun test-oieo (search-string refactor-string)
  (test-refactor-1 ada-refactor-object-index-to-element-object
		   ada-refactor-element-object-to-object-index
		   search-string refactor-string))

(advice-add #'run-test-here :after #'ada-test-casing)

(provide 'run-indent-test)
;; end of file
