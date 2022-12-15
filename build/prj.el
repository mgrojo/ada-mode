;; Project definitions for compiling ada-mode
(wisi-prj-select-cache
 "ada_mode_wisi_parse.prj"
 (create-ada-prj
  :name "ada_mode_wisi_parse main"
  :compile-env
  (list
   (concat "GNAT_COMPILER=" (expand-file-name "../../elpa/packages/gnat-compiler"))
   (concat "GPR_QUERY="     (expand-file-name "../../elpa/packages/gpr-query"))
   (concat "SAL="           (expand-file-name "../../org.stephe_leake.sal"))
   (concat "WISITOKEN="     (expand-file-name "../../org.wisitoken"))
   (concat "WISI=" 	    (expand-file-name "../../org.emacs.wisi"))
   ))
 "Makefile")

(ada-parse-require-process) ;; slow start due to lr1 parse table
;; end of file
