;; install wisitoken-grammar-mode, wisi from local elpa archive, for testing -*- no-byte-compile: t -*-

(require 'package)
(add-to-list 'package-archives (cons "test" "/Projects/elpa/archive-devel"))

;; wisi normally disabled in Stephe's early-init.el
(setq package-load-list '(all))

;; force update of test archive contents
(package-refresh-contents)

(package-initialize)
(setq package-check-signature nil)
(setq byte-compile-error-on-warn t)

(package-install 'wisitoken-grammar-mode)

(pop-to-buffer "*Messages*")
;; end of file
