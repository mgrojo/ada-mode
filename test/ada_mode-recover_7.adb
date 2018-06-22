-- From an actual editing session.
--
-- Missing ';' after 'end loop'.
--
-- Mckenzie encounters Unknown_State in a reduce during check of a
-- config returned by language_fixes.

package body WisiToken.LR.LR1_Items is
   procedure Follow
   is
   begin

      for B in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
      end loop

      Prev_Result := Result;
      return Result;
   end Follow;

end WisiToken.LR.LR1_Items;
