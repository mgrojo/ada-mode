--  Abstract :
--
--  Test of ada-format-paramlist when error correction happens
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_Format_Paramlist is

   -- Error correction leaves empty parameter declaration. The error
   -- correction leaves 'is begin' on the same line as the closing paren.
   --
   --EMACSCMD:(progn (end-of-line 5)(delete-char -1)(insert ";")(ada-align)(newline-and-indent))
   procedure Check_One
     (Label    : in String;
      Param    : in Integer;
      Expected : in Integer)
   is begin
      null;
   end Check_One;

end Ada_Mode.Recover_Format_Paramlist;
