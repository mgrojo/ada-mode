--  Separate spec file to test that xref-find-definitions goes to the correct file.
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
function Ada_Mode.Library_Function return Integer; -- spec

--EMACSCMD:(progn (forward-line -3)(test-all-defs "Library_Function"))
--EMACSRESULT_START:(cl-ecase ada-xref-backend (eglot (list "ada_mode-library_function.adb" "function Ada_Mode.Library_Function return Integer is")) (gpr_query (list "ada_mode-library_function.ads" "Library_Function function")) (gnat (list "ada_mode-library_function.ads" "Library_Function spec")))
--EMACSRESULT_ADD:(cl-ecase ada-xref-backend (eglot nil) (gpr_query (list "ada_mode-library_function.adb" "Library_Function body"))(gnat (list "ada_mode-library_function.adb" "Library_Function body")))
--EMACSRESULT_FINISH:
