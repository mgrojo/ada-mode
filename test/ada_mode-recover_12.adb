--  From a real editing session.
--
--  Missing 'end if;'
--
--  Used to violate an assert in recover, now finds a good solution quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_12 is
   function Recursive (Nonterm : in Token_Id; Prod : in Wisitoken.Productions.Instance) return Boolean
   is
   begin
   Rhs_Loop :
      for Rhs of Prod.Rhss loop
      Id_Loop :
         for Id of Rhs.Tokens loop
            if Id = Nonterm then
               return True;
            else
               if Id in Nonterminal then
                  exit Id_Loop;
               end if;
            end loop Id_Loop;
      end loop Rhs_Loop;
   end Recursive;
end Ada_Mode.Recover_12;
