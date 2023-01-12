;; Emacs wisi project definitions for compiling wisitoken with Alire -*- no-byte-compile: t; -*-

(wisi-prj-select-cache
 "prj.el"
 (create-alire-prj
  :name     "wisitoken alire"
  :gpr-file "build/wisitoken_alire.gpr"
  :xref-label 'gpr_query)
 "Makefile"
 )

(ada-parse-require-process) ;; slow start due to lr1 parse table
;; end of file
