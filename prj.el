;; Project definitions for compiling wisitoken_grammar_mode_parse.adb -*- no-byte-compile: t -*-

(wisi-prj-select-cache
 "wisitoken_grammar.prj"
 (create-ada-prj
  :name "wisitoken_grammar stephe-1"
  :compile-env
  (cl-ecase system-type
    (gnu/linux
     (list
      "MMM_MODE=/Projects/mmm-mode"
      "SAL=/Projects/org.stephe_leake.sal.stephe-1"
      "WISITOKEN=/Projects/org.wisitoken.stephe-1"
      "WISI=/Projects/org.emacs.wisi.stephe-1"))
    (windows-nt
     (list
      "MMM_MODE=c:/Projects/mmm-mode"
      "SAL=c:/Projects/org.stephe_leake.sal.stephe-1"
      "WISITOKEN=c:/Projects/org.wisitoken.stephe-1"
      "WISI=c:/Projects/org.emacs.wisi.stephe-1"))
    ))
  "Makefile")

;; end of file
