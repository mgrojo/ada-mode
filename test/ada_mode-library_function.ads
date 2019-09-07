--  Separate spec file to test that wisi-goto-declaration goes to the correct file.
--EMACSCMD:(wisi-prj-select-cached "subdir/ada_mode.adp" (ada-prj-default))
function Ada_Mode.Library_Function return Integer; -- spec

--EMACSCMD:(progn (forward-line -2)(forward-word 4)(forward-char 1)(wisi-goto-declaration)(looking-at "Library_Function return Integer is"))
--EMACSRESULT:t
