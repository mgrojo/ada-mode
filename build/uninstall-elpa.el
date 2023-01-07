;; uninstall ada-mode, wisi from ~/.emacs.d/elpa, for
;; reinstall with different emacs version

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

(defvar ada-mode-version (getenv "ADA_MODE_VERSION"))
(defvar ada-ref-man-version (getenv "ADA_REF_MAN_VERSION"))

(pkg-dir-clean "ada-mode" ada-mode-version)
(pkg-dir-clean "ada-ref-man" ada-ref-man-version)
(pkg-dir-clean "wisi" "4.2.0")
(pkg-dir-clean "uniquify-files" "1.0.4")

;; end of file
