;; Project definitions for compiling gpr-mode

(wisi-prj-select-cache
 "gpr_mode_wisi_parse.prj"
 (create-ada-prj
  :name "gpr_mode_wisi_parse main"
  :compile-env
  (list
   (concat "GNAT_COMPILER=" (expand-file-name "../../elpa/packages/gnat-compiler"))
   (concat "SAL=" (expand-file-name "../../org.stephe_leake.sal"))
   (concat "WISITOKEN=" (expand-file-name "../../org.wisitoken"))
   (concat "WISI=" (expand-file-name "../../org.emacs.wisi"))
   ))
 "Makefile"
 )

;; end of file
