-- Test that an ambiguous parse error is handled properly
--
-- There are two parallel parsers at start of error recovery; both
-- find solutions. Then at EOF there are two parsers active, both
-- accepting. However, since there was an error previously, rather
-- than reporting ambiguous parse as an error, we pick one parser
-- arbitrarily to allow indent.

--EMACS_SKIP_UNLESS: (eq ada-parser 'process)
--EMACSCMD:(wisi-parse-buffer 'indent)
procedure Ada_Mode.Ambiguous_Parse is
begin
   --EMACSCMD:(length (wisi-parser-parse-errors wisi--parser))
   --EMACSRESULT:1
   --EMACSCMD:wisi-parse-failed
   --EMACSRESULT:nil
