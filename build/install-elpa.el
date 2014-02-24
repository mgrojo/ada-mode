;; install ada-mode, wisi from local elpa archive, for testing

(setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))

(require 'package) ;; not loaded in batch mode
(package-initialize)

(setq ada-version "5.0.2")
(setq wisi-version "1.0.1")

;; WORKAROUND: In Emacs 24.3, if don't restart Emacs after
;; package-delete, package-installed-p still reports true.
(when (and (package-installed-p 'ada-mode (version-to-list ada-version))
	   (package--dir "ada-mode" ada-version))
  (package-delete "ada-mode" ada-version))

(when (and (package-installed-p 'wisi (version-to-list wisi-version))
	   (package--dir "wisi" wisi-version))
  (package-delete "wisi" wisi-version))

(package-download-tar 'wisi wisi-version)
(package-download-tar 'ada-mode ada-version)

;; end of file
