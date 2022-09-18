;; Emacs wisi project definitions for compiling ada-mode in ELPA workspace -*- no-byte-compile: t; -*-
;;
;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

(wisi-prj-select-cache
   "gpr-query.prj"
   (create-ada-prj :name "gpr-query elpa")
   "Makefile"
   ))

(ada-parse-require-process) ;; slow start due to lr1 parse table
;; end of file
