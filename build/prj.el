;; Project definitions for compiling gpr-mode
(require 'wisi-prj)
(require 'gnat-core)

(unless (memq #'wisi-prj-find-dominating-cached project-find-functions)
  (add-hook 'project-find-functions #'wisi-prj-find-dominating-cached -10)
  (add-hook 'xref-backend-functions #'wisi-prj-xref-backend 10))

(let ((wisitoken-core "/Projects/org.wisitoken")
      (wisi-core "/Projects/org.emacs.wisi"))
  (wisi-prj-select-cache
   "gpr_mode_wisi_parse.prj"
   (make-wisi-prj
    :name "gpr_mode_wisi_parse main"
    :compile-env
    (append
     (list "SAL=../../org.stephe_leake.sal")
     (cl-ecase system-type
       (gnu/linux
	(list
	 (concat "WISITOKEN=" wisitoken-core)
	 (concat "WISI=" wisi-core)))
       (windows-nt
	(list
	 (concat "WISITOKEN=c:" wisitoken-core)
	 (concat "WISI=c:" wisi-core))))
     )
    :compiler
    (create-gnat-compiler)) ;; sets gpr_project_path
   "Makefile"
   ))

(wisi-prj-set-dominating
 "gpr_mode_wisi_parse.prj"
 (make-wisi-prj
     :compiler
    (create-gnat-compiler))
 "Makefile")

;; end of file
