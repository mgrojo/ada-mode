;; Project definitions for compiling wisitoken_grammar_mode_parse.adb -*- no-byte-compile: t -*-

(wisi-prj-select-cache
 "wisitoken_grammar.prj"
 (create-ada-prj
  :name "wisitoken_grammar main"
  :compile-env
  (cl-ecase system-type
    (gnu/linux
     (list
      "MMM_MODE=/Projects/mmm-mode"
      "SAL=/Projects/org.stephe_leake.sal"
      "WISITOKEN=/Projects/org.wisitoken"
      "WISI=/Projects/org.emacs.wisi"))
    (windows-nt
     (list
      "MMM_MODE=c:/Projects/mmm-mode"
      "SAL=c:/Projects/org.stephe_leake.sal"
      "WISITOKEN=c:/Projects/org.wisitoken"
      "WISI=c:/Projects/org.emacs.wisi"))
    ))
  "Makefile")

;; end of file
