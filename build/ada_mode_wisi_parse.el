;; Project definitions for compiling ada_mode_wisi_parse.adb

(require 'ada-project)
(require 'xref-ada)

(let ((prj-file "ada_mode_wisi_parse.prj"))
  (ada-parse-prj-file prj-file)
  (ada-select-prj-file prj-file)
  (push (cons default-directory (cons 'ada-mode (concat default-directory prj-file))) sal-project-dir-alist))

(setq project-find-functions '(project-menu-prj))

;; Ada mode adds another layer of project selection
(project-menu-select "Ada mode")

;; end of file
