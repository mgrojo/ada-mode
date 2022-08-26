;; Project definitions for compiling gpr-mode

(unless (memq #'wisi-prj-find-dominating-cached project-find-functions)
  (add-hook 'project-find-functions #'wisi-prj-find-dominating-cached -10)
  (add-hook 'xref-backend-functions #'wisi-prj-xref-backend 10))

(wisi-prj-select-cache
 "gpr_mode_wisi_parse.prj"
 (create-ada-prj
  :name "gpr_mode_wisi_parse main"
  :compile-env
  (list
   (concat "GPR_COMPILER=" (expand-file-name "../../org.emacs.gpr-compiler"))
   (concat "SAL=" 	   (expand-file-name "../../org.stephe_leake.sal.stephe-1"))
   (concat "WISITOKEN="    (expand-file-name "../../org.wisitoken.stephe-1"))
   (concat "WISI=" 	   (expand-file-name "../../org.emacs.wisi.stephe-4"))
   ))
 "Makefile")

;; end of file
