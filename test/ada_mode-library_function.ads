--  Separate spec file to test that ada-goto-declaration goes to the correct file.
--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
function Ada_Mode.Library_Function return Integer; -- spec

--EMACSCMD:(progn (forward-line -2)(forward-word 4)(forward-char 1)(ada-goto-declaration)(looking-at "Library_Function return Integer is"))
--EMACSRESULT:t
