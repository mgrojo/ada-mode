--  From a real editing session. Used to throw CONSTRAINT_ERROR, now fixed.

--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_04 is
   procedure Handle_Search
   is begin
      for Parser_State of Shared_Parser.Parsers loop
         if Verb = Shift_Recover and  or else --  error here; extra "or else"
           Resume_Token_Goal <= Shared_Token
         then
            if Parser_State > 0 and then
              Parser_State = Insert
            then
               Parser_State := 1;
            else
               Parser_State.Set_Verb (Shift);
            end if;
         end if;
      end loop;
   end Handle_Search;
end Ada_Mode.Recover_04;
