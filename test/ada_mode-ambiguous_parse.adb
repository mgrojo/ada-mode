-- Test that an ambiguous parse error is handled properly
--
-- There are two parallel parsers at EOF, so there is an error from
-- each of those.

--EMACSCMD:(wisi-parse-buffer)
procedure Ada_Mode.Ambiguous_Parse is
begin
   --EMACSCMD:(length (wisi-parser-errors wisi--parser))
   --EMACSRESULT:2
