;; Project definitions for compiling ada-mode
(wisi-prj-select-cache
 "ada_mode_wisi_parse.prj"
 (create-ada-prj
  :name "ada_mode_wisi_parse main"
  :compile-env
  (list
   (concat "SAL="       (expand-file-name "../../org.stephe_leake.sal.stephe-2"))
   (concat "WISITOKEN=" (expand-file-name "../../org.wisitoken.stephe-3"))
   (concat "WISI="      (expand-file-name "../../org.emacs.wisi.stephe-3"))
   ))
 "Makefile"
 )

(ada-parse-require-process) ;; slow start due to lr1 parse table
;; end of file
