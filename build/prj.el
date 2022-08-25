;; Project definitions for compiling gpr-mode

(let ((wisitoken-core "/Projects/org.wisitoken")
      (wisi-core "/Projects/org.emacs.wisi"))
  (wisi-prj-select-cache
   "gpr_mode_wisi_parse.prj"
   (make-wisi-prj
    :name "gpr_mode_wisi_parse main"
    :compile-env
    (list
     (concat "SAL=" (expand-file-name "../../org.stephe_leake.sal"))
     (concat "WISITOKEN=" (expand-file-name "../org.wisitoken"))
     (concat "WISI=" (expand-file-name "../org.emacs.wisi")))
    :compiler (create-gnat-compiler)) ;; sets gpr_project_path
   "Makefile"
   ))

;; end of file
