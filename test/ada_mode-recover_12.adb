--  From a real editing session. Violates an assert: Match_Names_Error 1  37 /= 24 loop_statement"
package debug
   function Recursive (Nonterm : in Token_ID; Prod : in WisiToken.Productions.Instance) return Boolean
   is
   begin
   RHS_Loop :
      for RHS of Prod.RHSs loop
      ID_Loop :
         for ID of RHS.Tokens loop
            if ID = Nonterm then
               return True;
            else
               if ID in Nonterminal then
                  exit ID_Loop;
               end if;
         end loop ID_Loop;
      end loop RHS_Loop;
   end Recursive;
end Debug;
