;; install ada-mode, wisi from local elpa archive, for testing

(setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))

(require 'package) ;; not loaded in batch mode
(package-initialize)

(setq ada-version "5.1.0")
(setq wisi-version "1.0.1")

;; We may be installing a newer version without a version bump, so
;; just delete the packages.
(when (package--dir "ada-mode" ada-version)
  (package-delete "ada-mode" ada-version))

(when (package--dir "wisi" wisi-version)
  (package-delete "wisi" wisi-version))

(when (and (= emacs-major-version 24)
	   (= emacs-minor-version 2)
	   (not (package--dir "cl-lib" "0.4")))
  (package-refresh-contents)
  (package-install 'cl-lib)
  (package-initialize))

(package-download-tar 'wisi wisi-version)
(package-download-tar 'ada-mode ada-version)

;; end of file
