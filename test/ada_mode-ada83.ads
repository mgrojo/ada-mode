-- Test casing and font-lock with on keywords added in later language versions.
--
-- We only test the face of one keyword from each of the
-- ada-*-casing-versions; enough to tell that the choice via file
-- local variable works.

--EMACSCMD:(setq skip-reindent-test t)
--EMACSCMD:(setq case-fold-search nil)

-- global-font-lock-mode is off in batch tests
--EMACSCMD:(font-lock-fontify-buffer)

-- from ada83
--EMACSCMD:(test-face "abort" 'font-lock-keyword-face)
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(ada-case-adjust)(looking-back "'abort"))
abort

-- from ada95
--EMACSCMD:(test-face "Abstract" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(ada-case-adjust)(looking-back "'Abstract"))
Abstract

-- from ada2005
--EMACSCMD:(test-face "Interface" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(ada-case-adjust)(looking-back "'Interface"))
Interface

-- from ada2012
--EMACSCMD:(test-face "Some" '(nil default))
--EMACSCMD:(progn (forward-line 1)(upcase-word 1)(ada-case-adjust)(looking-back "'Some"))
Some

-- Local Variables:
-- ada-language-version : ada83
-- End:
