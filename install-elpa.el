;; install wisitoken-grammar-mode, wisi from local elpa archive, for testing -*- no-byte-compile: t -*-

(require 'package)
(add-to-list 'package-archives (cons "test" "/Projects/elpa/archive/packages"))

(package-initialize)
(setq package-check-signature nil)
(setq byte-compile-error-on-warn t)

(setq wisitoken-grammar-mode-version (getenv "WISITOKEN_GRAMMAR_MODE_VERSION"))
(setq wisi-version (getenv "WISI_VERSION"))

(defun pkg-dir (name version)
  (concat (locate-user-emacs-file "elpa") "/" name "-" version))

(defun pkg-download (name version &optional archive kind)
  (let ((default-directory (pkg-dir name version))
	(pkg-desc
	 (package-desc-create
	  :name (intern name)
	  :version (version-to-list version)
	  :kind (or kind 'tar)
	  :archive (or archive "test"))))
    (package-install-from-archive pkg-desc)))

(pkg-download "wisitoken-grammar-mode" wisitoken-grammar-mode-version)

;; end of file
