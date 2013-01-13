;; set up for running `semantic-grammar-create-package'
;; assumes current directory contains ada-mode.el

;; Emacs 24.3 contains all of semantic; no need to add to load-path

;; FIXME: was needed by wisi? (require 'cl)
(require 'semantic/grammar)
(setq wisent-verbose-flag t); for fixing errors

;;;; end of file
