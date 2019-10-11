--  Separate spec file to test that wisi-goto-declaration goes to the correct file.
--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "subdir/ada_mode.adp") (gnatxref "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
function Ada_Mode.Library_Function return Integer; -- spec

--EMACSCMD:(progn (forward-line -2)(forward-word 4)(forward-char 1)(wisi-goto-declaration)(looking-at "Library_Function return Integer is"))
--EMACSRESULT:t
