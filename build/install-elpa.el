;; install ada-mode, wisi from local elpa archive, for testing

(require 'package)
(add-to-list 'package-archives (cons "test" "/Projects/elpa/archive/packages"))

(package-initialize)

(setq ada-mode-version "5.2.1")
(setq ada-ref-man-version "2012.3")
(setq wisi-version "1.1.4")

(cond
 ;; package handler details change between emacs versions
 ((string-equal emacs-version "24.2.1")
  (package-install 'cl-lib)
  (load-file "install-elpa-24.3.el"))

 ((string-equal emacs-version "24.3.1")
  (load-file "install-elpa-24.3.el"))

 ((or (string-equal emacs-version "24.4.1")
      (string-equal emacs-version "24.5.1")
      (= emacs-major-version 25))
  (load-file "install-elpa-24.4.el"))

 (t
  (error "install-elpa.el: unsupported emacs-version"))
 )
;; end of file
