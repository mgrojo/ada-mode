;; Project definitions for compiling wisi_grammar_mode_parse.adb -*- no-byte-compile: t -*-

(require 'ada-project)
(require 'xref-ada)

(setq project-find-functions '(project-menu-prj))

(let* ((prj-file "wisi_grammar.prj")
       (prj-name "wisi_grammar stephe-1")
       (prj (make-ada-project
	     :env-vars '(("WISITOKEN" . "c:/Projects/org.wisitoken.stephe-1/build")
			 ("WISI" . "c:/Projects/org.emacs.ada-mode.stephe-4"))
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )

;; end of file
