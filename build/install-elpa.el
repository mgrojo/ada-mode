;; install ada-mode, wisi from local elpa archive, for testing

(require 'package)
(add-to-list 'package-archives (cons "test" "/Projects/elpa/archive-devel"))

(when (eq system-type 'windows-nt)
  ;; Default uses "c:/"; mingw gpg requires "/c/"
  (setq package-gnupghome-dir "/c/home/stephe/.emacs.d/elpa/gnupg"))

;; gpr-mode, wisi normally disabled in Stephe's early-init.el
(setq package-load-list '(all))

;; force update of test archive contents
(package-refresh-contents)

(package-initialize)
(setq package-check-signature nil)
(setq byte-compile-error-on-warn t)

(package-install 'gpr-mode)

(pop-to-buffer "*Messges*")
;; end of file
