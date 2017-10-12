;; Project definitions for compiling test *_wisi_parse.adb

(require 'ada-project)
(require 'xref-ada)

(ada-parse-prj-file "wisi_parse.prj")
(ada-select-prj-file "wisi_parse.prj")

(setq project-find-functions '(project-menu-prj))

;; Ada mode adds another layer of project selection
(project-menu-select "Ada mode")

;; end of file
