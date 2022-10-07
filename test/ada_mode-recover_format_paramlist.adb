--  Abstract :
--
--  Test of ada-format-paramlist when error correction happens
--
--EMACSCMD:(setq wisi-indent-region-fallback nil)
--EMACSCMD:(setq skip-reindent-test t)
package body Ada_Mode.Recover_Format_Paramlist is

   -- IMPROVEME: currently ada-align does not invoke wisi-repair-error;
   -- perhaps it should? Collect real use cases.

   --EMACSCMD:(progn (forward-line 4)(ada-align))
   procedure Check_One
     (Label : in String;
      Param : in Integer;
            : in Integer)
   is begin
      null;
   end Check_One;

end Ada_Mode.Recover_Format_Paramlist;
