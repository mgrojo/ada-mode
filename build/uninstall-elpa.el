;; uninstall install ada-mode, wisi from local elpa archive, for
;; reinstall with different emacs version

(require 'package)

(package-initialize)

(setq ada-mode-version (getenv "ADA_MODE_VERSION"))
(setq ada-ref-man-version (getenv "ADA_REF_MAN_VERSION"))
(setq wisi-version (getenv "WISI_VERSION"))

(cond
 ;; package handler details change between emacs versions
 ((string-equal emacs-version "24.2.1")
  (when (package--dir "cl-lib" "0.5")
    (package-delete "cl-lib" "0.5"))
  (load-file "uninstall-elpa-24.3.el"))

 ((string-equal emacs-version "24.3.1")
  (load-file "uninstall-elpa-24.3.el"))

 ((or (string-equal emacs-version "24.4.1")
      (string-equal emacs-version "24.5.1")
      (= emacs-major-version 25))
  (load-file "uninstall-elpa-24.4.el"))


 (t
  (error "install-elpa.el: unsupported emacs-version"))
 )
;; end of file
