--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.Generate is

   function Has_Empty_Production
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return Token_ID_Set
   is
      use all type Ada.Containers.Count_Type;

      subtype Nonterminal is Token_ID range Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal;

      Result  : Token_ID_Set := (Nonterminal => False);
      Changed : Boolean      := True;
   begin
      --  First check consistency; Has_Empty_Production is the first
      --  subprogram to process a grammar.
      if Descriptor.Accept_ID /= Descriptor.First_Nonterminal then
         raise Grammar_Error with "Descriptor.Accept_ID /= Descriptor.First_Nonterminal";
      end if;
      if Grammar.First_Index /= Descriptor.First_Nonterminal then
         raise Grammar_Error with "Grammar.First_Index /= Descriptor.First_Nonterminal";
      end if;
      if Grammar.Last_Index /= Descriptor.Last_Nonterminal then
         raise Grammar_Error with "Grammar.Last_Index /= Descriptor.Last_Nonterminal";
      end if;

      for Nonterm in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal loop
         if Grammar (Nonterm).LHS /= Nonterm then
            raise Grammar_Error with "Grammar (" & Image (Nonterm, Descriptor) & ").LHS /= " &
              Image (Nonterm, Descriptor);
         end if;
      end loop;

      loop
         exit when not Changed;
         Changed := False;

         for Prod of Grammar loop
            for RHS of Prod.RHSs loop
               if (RHS.Tokens.Length = 0 or else
                     (RHS.Tokens (1) in Nonterminal and then Result (RHS.Tokens (1)))) and
                 not Result (Prod.LHS)
               then
                  Result (Prod.LHS) := True;
                  Changed := True;
               end if;
            end loop;
         end loop;
      end loop;
      return Result;
   end Has_Empty_Production;

end WisiToken.Generate;
