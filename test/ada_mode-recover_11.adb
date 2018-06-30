--  From a real editing session.
--
--  Encounters a bug in wisitoken-lr-mckenzie_recover-ada.adb Match_Names_Error

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
               for I in reverse RHS.Tokens.First_Index + 1 .. RHS.Tokens.Last_Index loop
                  declare

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
