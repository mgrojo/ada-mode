;; Project definitions for compiling ada_mode_wisi_parse.adb

(require 'ada-core)
(require 'wisi-prj)

(let ((wisitoken-core "/Projects/org.wisitoken.stephe-1"))

  (wisi-prj-set-dominating
   "Makefile"
   "ada_mode_wisi_parse.prj"
   (make-ada-prj
    :name "ada_mode_wisi_parse stephe-1"
    :compile-env
    (append
     (list "SAL=../../org.stephe_leake.sal.stephe-1")
     (cl-ecase system-type
      (gnu/linux
       (list
	(concat "WISITOKEN=" wisitoken-core)
	"LIBADALANG=/Projects/libadalang/build/lib/gnat"))
      (windows-nt
       (list
	(concat "WISITOKEN=c:" wisitoken-core))))
      ))
  ))

;; end of file
