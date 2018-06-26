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

package body WisiToken.Generate.Packrat is

   function Potential_Direct_Left_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set
   is
      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;
   begin
      return Result : Token_ID_Set (Nonterminal) := (others => False) do
         for Prod of Grammar loop
            RHS_Loop :
            for RHS of Prod.RHSs loop
               ID_Loop :
               for ID of RHS.Tokens loop
                  if ID = Prod.LHS then
                     Result (ID) := True;
                     exit RHS_Loop;
                  elsif not (ID in Nonterminal) then
                     exit ID_Loop;
                  elsif not Empty (ID) then
                     exit ID_Loop;
                  end if;
               end loop ID_Loop;
            end loop RHS_Loop;
         end loop;
      end return;
   end Potential_Direct_Left_Recursive;

   function Potential_Direct_Right_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set
   is
      subtype Nonterminal is Token_ID range Grammar.First_Index .. Grammar.Last_Index;
   begin
      return Result : Token_ID_Set (Nonterminal) := (others => False) do
         for Prod of Grammar loop
            RHS_Loop :
            for RHS of Prod.RHSs loop
               ID_Loop :
               for ID of reverse RHS.Tokens loop
                  if ID = Prod.LHS then
                     Result (ID) := True;
                     exit RHS_Loop;
                  elsif not (ID in Nonterminal) then
                     exit ID_Loop;
                  elsif not Empty (ID) then
                     exit ID_Loop;
                  end if;
               end loop ID_Loop;
            end loop RHS_Loop;
         end loop;
      end return;
   end Potential_Direct_Right_Recursive;

end WisiToken.Generate.Packrat;
