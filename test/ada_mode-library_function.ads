--  Separate spec file to test that ada-goto-declaration goes to the correct file.
function Ada_Mode.Library_Function return Integer; -- spec
--EMACSCMD:(progn (forward-line -1)(forward-word 2)(forward-char 1)(ada-goto-declaration nil)(looking-at "Library_Function return Integer is"))
--EMACSRESULT:t
