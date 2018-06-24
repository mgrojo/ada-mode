--  From a real editing session.
--
--  Added "declare" in inner loop.
--
--  Encounters an assertion failure in language fixes Match_Names_Error
package body WisiToken.LR.LR1_Items is
   procedure Closure
   is
   begin
               For_Each_Production :
               for Prod of Grammar loop
                  For_Each_RHS :
                  for B in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                     declare

                     if Prod.LHS = RHS.Tokens (Item.Dot) then

                        Beta := Next (Item.Dot);
                     end if;
                  end loop For_Each_RHS;
               end loop For_Each_Production;

   end Closure;

end WisiToken.LR.LR1_Items;
