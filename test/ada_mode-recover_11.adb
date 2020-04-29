--  From a real editing session.
--
--  Extra 'declare'; just started adding a declare block.
--
-- Used to encounter a bug in wisitoken-lr-mckenzie_recover-ada.adb
-- Match_Names_Error; now finds a good solution quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
function Ada_Mode.Recover_11
  (Grammar : in Wisitoken.Productions.Prod_Arrays.Vector;
   Empty   : in Token_Id_Set)
  return Token_Id_Set
is
   subtype Nonterminal is Token_Id range Grammar.First_Index .. Grammar.Last_Index;
begin
   return Result : Token_Id_Set (Nonterminal) := (others => False) do
      for Prod of Grammar loop
      Rhs_Loop :
         for Rhs of Prod.Rhss loop
         Id_Loop :
            for I in reverse Rhs.Tokens.First_Index + 1 .. Rhs.Tokens.Last_Index loop
               declare

               if Id = Prod.Lhs then
                     Result (Id) := True;
                     exit Rhs_Loop;
                  elsif not (Id in Nonterminal) then
                     exit Id_Loop;
                  elsif not Empty (Id) then
                     exit Id_Loop;
                  end if;
               end loop Id_Loop;
         end loop Rhs_Loop;
      end loop;
   end return;
end Ada_Mode.Recover_11;
