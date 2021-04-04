-- Test casing and font-lock on keywords added in later language versions.
--
-- We only test the face of one keyword from each of the
-- ada-*-casing-versions; enough to tell that the choice via file
-- local variable works. The face is set by font-lock-keywords, not
-- the parser.

--EMACSCMD:(setq skip-reindent-test t)
--EMACSCMD:(setq case-fold-search nil)

-- from ada83
--EMACSCMD:(test-face "abort" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'abort"))
abort

-- from ada95
--EMACSCMD:(test-face "abstract" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'abstract"))
abstract

-- from ada2005
--EMACSCMD:(test-face "Interface" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'Interface"))
Interface

-- from ada2012
--EMACSCMD:(test-face "Some" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(wisi-case-adjust)(looking-back "'Some"))
Some

-- The parser fails, since this is really bad syntax; don't abort for
-- that.
-- Local Variables:
-- wisi-debug: 0
-- ada-language-version: ada95
-- wisi-mckenzie-task-count: 1
-- End:
