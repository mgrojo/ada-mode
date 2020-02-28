package body Ada_Mode.Recover_4 is
   procedure Handle_Search
   is begin
      for Parser_State of Shared_Parser.Parsers loop
         if Verb = Shift_Recover and
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
end Ada_Mode.Recover_4;
