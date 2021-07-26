;; Project definitions for compiling ada-mode

(let ((wisitoken-core "/Projects/org.wisitoken")
      (wisi-core "/Projects/org.emacs.wisi"))

  (wisi-prj-select-cache
   "ada_mode_wisi_parse.prj"
   (create-ada-prj
    :name "ada_mode_wisi_parse main"
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
      ))
   "Makefile"
   ))

;; end of file
