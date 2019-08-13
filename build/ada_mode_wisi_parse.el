;; Project definitions for compiling ada_mode_wisi_parse.adb

(cond
 ((or
   (< emacs-major-version 25) ;; not worth fixing
   (not (and (locate-library "ada-project")
	     (condition-case nil (require 'ada-mode nil t) (error nil))
	     (fboundp 'ada-deselect-prj) ;; in 6.0.0
	     )))
  ;; just define WISITOKEN, GPR_PROJECT_PATH so Makefile works
  (cl-ecase system-type
    (gnu/linux
     (setenv "WISITOKEN" "/Projects/org.wisitoken")
     (setenv "GPR_PROJECT_PATH"
	     (concat
	      (getenv "WISITOKEN") "/build"
	      ":" "/Projects/org.stephe_leake.aunit_ext/build"
	      ":" "/Projects/org.stephe_leake.sal/build"
	      ":" "/Projects/org.stephe_leake.makerules"))
     )
    (windows-nt
     (setenv "WISITOKEN" "c:/Projects/org.wisitoken")
     (setenv "GPR_PROJECT_PATH"
	     (concat
	      (getenv "WISITOKEN") "/build"
	      ";" "/Projects/org.stephe_leake.aunit_ext/build"
	      ";" "/Projects/org.stephe_leake.sal/build"
	      ";" "/Projects/org.stephe_leake.makerules"))
     )
    )
  )

 (t
(require 'ada-project)
(require 'project-menu)
(require 'xref-ada)
(require 'wisi)

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file (expand-file-name "ada_mode_wisi_parse.prj"))
       (prj-name "ada_mode_wisi_parse main")
       (prj (make-ada-project
	     :env-vars
	     (cl-ecase system-type
	       (gnu/linux
		'(("WISITOKEN" . "/Projects/org.wisitoken")
		  ("LIBADALANG" . "/Projects/libadalang/build/lib/gnat")))
	       (windows-nt
		'(("WISITOKEN" . "c:/Projects/org.wisitoken")))
	       )
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )
))
;; end of file
