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

(provide 'run-indent-test)
;; end of file
