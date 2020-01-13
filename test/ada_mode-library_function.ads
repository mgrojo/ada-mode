--  Separate spec file to test that xref-find-definitions goes to the correct file.
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
function Ada_Mode.Library_Function return Integer; -- spec

--EMACSCMD:(progn (forward-line -3)(test-all-defs "Library_Function"))
--EMACSRESULT_START:'("ada_mode-library_function.ads" "Library_Function function")
--EMACSRESULT_ADD:'("ada_mode-library_function.adb" "Library_Function body")
--EMACSRESULT_FINISH:
