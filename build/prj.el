;; Project definitions

(require 'ada-project)
(require 'xref-ada)

(add-to-list 'project-find-functions 'project-menu-prj)

(let* ((prj-file "sal.prj")
       (prj-name "sal main")
       (prj (make-ada-project
	     :env-vars nil
	     :ada-prj prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )
;; end of file
