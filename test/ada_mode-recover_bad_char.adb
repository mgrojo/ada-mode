--  Abstract :
--
--  An error from a real editing session; typed '"' instead of '?'.
--
--  Lexer now reports bad characters, and recovers by skipping them.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

package Ada_Mode.Recover_Bad_Char is
   A : String := "(?:[0-9]{5} "\(?([A-La-l]))|";

end Ada_Mode.Recover_Bad_Char;
