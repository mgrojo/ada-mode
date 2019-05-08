;; Project definitions for compiling wisitoken_grammar_mode_parse.adb -*- no-byte-compile: t -*-

(require 'ada-project)
(require 'project-menu)
(require 'xref-ada)

(add-to-list 'project-find-functions #'project-menu-prj)

(let* ((prj-file "wisitoken_grammar.prj")
       (prj-name "wisitoken_grammar main")
       (prj (make-ada-project
	     :env-vars
	     (cl-ecase system-type
	       (gnu/linux
		'(("WISITOKEN" . "/Projects/org.wisitoken")
		  ("EMACS_WISI" . "/Projects/org.emacs.ada-mode")))
	       (windows-nt
		'(("WISITOKEN" . "c:/Projects/org.wisitoken")
		  ("EMACS_WISI" . "c:/Projects/org.emacs.ada-mode")))
	       )
	     :ada-prj-file prj-file)))

  (project-menu-add-project prj prj-name default-directory)

  (project-menu-select-by-name prj-name)
  )

;; end of file
