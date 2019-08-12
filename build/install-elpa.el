;; install ada-mode, wisi from local elpa archive, for testing

(require 'package)
(add-to-list 'package-archives (cons "test" "/Projects/elpa/archive/packages"))

(package-initialize)
(setq package-check-signature nil)
(setq byte-compile-error-on-warn t)

(setq ada-mode-version (getenv "ADA_MODE_VERSION"))
(setq ada-ref-man-version (getenv "ADA_REF_MAN_VERSION"))
(setq wisi-version (getenv "WISI_VERSION"))
(setq uniquify-files-version (getenv "UNIQUIFY_FILES_VERSION"))

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

;; download in dependency order

(pkg-download "uniquify-files" uniquify-files-version)
(pkg-download "wisi" wisi-version)
(pkg-download "ada-mode" ada-mode-version)
(pkg-download "ada-ref-man" ada-ref-man-version)

;; end of file
