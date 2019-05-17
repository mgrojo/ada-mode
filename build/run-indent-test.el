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

(advice-add #'run-test-here :after #'ada-test-casing)

(provide 'run-indent-test)
;; end of file
