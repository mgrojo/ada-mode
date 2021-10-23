;; Project definitions for working on wisi

(let ((wisitoken-core "/Projects/org.wisitoken"))

  (wisi-prj-select-cache
   "wisi.prj"
   (create-ada-prj
    :name "wisi main"
    :compile-env
    (append
     (list "SAL=../../org.stephe_leake.sal")
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
