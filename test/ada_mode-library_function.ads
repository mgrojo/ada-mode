--  Separate spec file to test that xref-find-definitions goes to the correct file.
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
function Ada_Mode.Library_Function return Integer; -- spec

--EMACSCMD:(progn (forward-line -3)(test-all-defs "Library_Function"))
--EMACSRESULT_START:(list "ada_mode-library_function.ads" (concat "Library_Function "(cl-ecase ada-xref-backend ((gpr_query eglot) "function")(gnat "spec"))))
--EMACSRESULT_ADD:(list "ada_mode-library_function.adb" (concat "Library_Function "(cl-ecase ada-xref-backend ((gpr_query eglot) "body")(gnat "body"))))
--EMACSRESULT_FINISH:
