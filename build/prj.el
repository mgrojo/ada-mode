;; Project definitions for compiling ada-mode

(let ((wisitoken-core "/Projects/org.wisitoken.stephe-2"))

  (wisi-prj-select-cache
   "ada_mode_wisi_parse.prj"
   (create-ada-prj
    :name "ada_mode_wisi_parse stephe-6"
    :compile-env
    (append
     (list "SAL=../../org.stephe_leake.sal.stephe-1")
     (cl-ecase system-type
      (gnu/linux
       (list
	(concat "WISITOKEN=" wisitoken-core)))
      (windows-nt
       (list
	(concat "WISITOKEN=c:" wisitoken-core))))
      ))
   "Makefile"
   ))

;; end of file
