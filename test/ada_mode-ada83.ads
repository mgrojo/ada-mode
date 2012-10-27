-- Test font-lock with ada-language-version not ada2012

--EMACSCMD:(setq skip-reindent-test t)

--EMACSCMD:(setq ada-language-version 'ada83)
-- need to recompute font-lock-keywords and re-fontify
--EMACSCMD:(ada-mode)
--EMACSCMD:(font-lock-fontify-buffer)
--EMACSCMD:(test-face "abstract" 'default)
abstract

--EMACSCMD:(setq ada-language-version 'ada95)
--EMACSCMD:(ada-mode)
--EMACSCMD:(font-lock-fontify-buffer)
--EMACSCMD:(test-face "abstract" 'font-lock-keyword-face)
abstract
--EMACSCMD:(test-face "interface" 'default)
interface

--EMACSCMD:(setq ada-language-version 'ada2005)
--EMACSCMD:(ada-mode)
--EMACSCMD:(font-lock-fontify-buffer)
--EMACSCMD:(test-face "interface" 'font-lock-keyword-face)
interface
--EMACSCMD:(test-face "some" 'default)
some

--EMACSCMD:(setq ada-language-version 'ada2012)
--EMACSCMD:(ada-mode)
--EMACSCMD:(font-lock-fontify-buffer)
--EMACSCMD:(test-face "some" 'font-lock-keyword-face)
some
  
  
