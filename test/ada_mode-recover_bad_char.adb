-- An error from a real editing session; typed '"' instead of '?'.
-- There are several unrecognized chars as well as an unbalanced
-- string quote.
--
--  Lexer now reports bad characters, and recovers by skipping them or
--  inserting a matching quote.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

package Ada_Mode.Recover_Bad_Char is
   A : String := "(?:[0-9]{5} "\(?([A-La-l]))|";

end Ada_Mode.Recover_Bad_Char;
