-- Test casing and font-lock on keywords added in later language versions.
--
-- We only test the face of one keyword from each of the
-- ada-*-casing-versions; enough to tell that the choice via file
-- local variable works. The face is set by font-lock-keywords, not
-- the parser. We use valid 2012 syntax to keep the parser happy.

--EMACSCMD:(setq skip-reindent-test t)
--EMACSCMD:(setq case-fold-search nil)

-- from ada83
--EMACSCMD:(test-face "abort" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'abort"))
abort Name;

-- from ada95
--EMACSCMD:(test-face "abstract" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'Abstract"))
type Foo is abstract tagged null record;

-- from ada2005
--EMACSCMD:(test-face "interface" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'Interface"))
type Bar is interface;

-- from ada2012
--EMACSCMD:(test-face "some" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'Some"))
A := (for some I of B => B);

-- Local Variables:
-- ada-language-version : ada83
-- End:
