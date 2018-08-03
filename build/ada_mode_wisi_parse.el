;; Project definitions for compiling ada_mode_wisi_parse.adb

(require 'ada-project)
(require 'xref-ada)
(require 'wisi)

(setq-default wisi-size-threshold 900000)
;; libadalang files are really large.
;; FIXME: do this in the project

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file (expand-file-name "ada_mode_wisi_parse.prj"))
       (prj-name "ada_mode_wisi_parse stephe-5")
       (prj (make-ada-project
	     :env-vars
	     (cl-ecase system-type
	       (gnu/linux
		'(("WISI_WISITOKEN" . "/Projects/org.wisitoken.stephe-1/build")
		  ("LIBADALANG" . "/Projects/libadalang/build/lib/gnat")))
	       (windows-nt '(("WISI_WISITOKEN" . "c:/Projects/org.wisitoken.stephe-1/build")))
	       )
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )

;; end of file
