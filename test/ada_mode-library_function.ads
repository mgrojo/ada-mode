--  Separate spec file to test that xref-find-definitions goes to the correct file.
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
function Ada_Mode.Library_Function return Integer; -- spec

--EMACSCMD:(progn (forward-line -2)(forward-word 4)(forward-char 1)(xref-find-definitions (xref-backend-identifier-at-point (xref-find-backend)))(looking-at "Library_Function return Integer is"))
--EMACSRESULT:t
