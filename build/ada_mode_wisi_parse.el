;; Project definitions for compiling ada_mode_wisi_parse.adb

(require 'ada-project)
(require 'xref-ada)

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file (expand-file-name "ada_mode_wisi_parse.prj"))
       (prj-name "ada_mode_wisi_parse main")
       (prj (make-ada-project
	     :env-vars '(("WISI_WISITOKEN" . "c:/Projects/org.wisitoken/build"))
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )

;; end of file
