--  From a real editing session.
--
--  Added "declare" in inner loop.
--
-- Used to encounter an assertion failure in language fixes
-- Match_Names_Error; now finds a good solution quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_08 is
   procedure Closure
   is
   begin
   For_Each_Production :
      for Prod of Grammar loop
      For_Each_Rhs :
         for B in Prod.Rhss.First_Index .. Prod.Rhss.Last_Index loop
            declare

               if Prod.Lhs = Rhs.Tokens (Item.Dot) then

                  Beta := Next (Item.Dot);
               end if;
         end loop For_Each_Rhs;
      end loop For_Each_Production;

   end Closure;

end Ada_Mode.Recover_08;
