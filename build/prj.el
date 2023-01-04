;; Project definitions for working on wisi with gprbuild

(wisi-prj-select-cache
 "wisi.prj"
 (create-ada-prj
  :name "wisi main"
  :compile-env
  (list
   (concat "SAL="       (expand-file-name "../../org.stephe_leake.sal"))
   (concat "WISITOKEN=" (expand-file-name "../../org.wisitoken"))
   ))
 "Makefile"
 )

;; end of file
