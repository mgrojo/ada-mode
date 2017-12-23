;; Project definitions for compiling wisi_grammar_mode_parse.adb -*- no-byte-compile: t -*-

(require 'ada-project)
(require 'xref-ada)

(ada-parse-prj-file "wisi_grammar.prj")
(ada-select-prj-file "wisi_grammar.prj")

(setq project-find-functions '(project-menu-prj))

;; Ada mode adds another layer of project selection
(project-menu-select "Ada mode")

;; end of file
