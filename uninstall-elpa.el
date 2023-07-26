;; uninstall wisitoken-grammar-mode, wisi from ~/.emacs.d/elpa -*- no-byte-compile: t -*-
;; for reinstall with different emacs version

(require 'package)

(package-initialize)

(defun pkg-dir (name version)
  (concat (locate-user-emacs-file "elpa") "/" name "-" version))

(defun pkg-dir-clean (name version)
  (let* ((elpa-dir (expand-file-name (locate-user-emacs-file "elpa")))
	 (files (file-name-all-completions (concat name "-" version) elpa-dir)))
    (dolist (file files)
      (let ((file-name (expand-file-name file elpa-dir)))
	(if (file-directory-p file-name)
	    ;; This handles installing from local build of elpa, where
	    ;; the actual version has rev/date info appended.
	    (delete-directory file-name t)
	  (delete-file file-name))))))

(pkg-dir-clean "wisitoken-grammar-mode" (getenv "WISITOKEN_GRAMMAR_MODE_VERSION"))
(pkg-dir-clean "wisi" (getenv "WISI_VERSION"))

;; end of file
