-- Test casing and font-lock on keywords added in later language versions.
--
-- See comment in ada_mode-ada83.ads

--EMACSCMD:(setq skip-reindent-test t)
--EMACSCMD:(setq case-fold-search nil)

-- from ada83
--EMACSCMD:(test-face "abort" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'abort"))
abort Name;

-- from ada95
--EMACSCMD:(test-face "abstract" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'abstract"))
type Foo is abstract tagged null record;

-- from ada2005
--EMACSCMD:(test-face "interface" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'interface"))
type Bar is interface;

-- from ada2012
--EMACSCMD:(test-face "some" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'some"))
A := (for some I of B => B);

-- Local Variables:
-- ada-language-version : ada2012
-- End:
