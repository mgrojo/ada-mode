--  From a real editing session. Use report
--  (error "unhandled exception: CONSTRAINT_ERROR: sal-gen_unbounded_definite_vectors.adb:237 range check failed")

package body Debug is
   procedure Handle_Search
   is

   begin
      for Parser_State of Shared_Parser.Parsers loop
               if Verb = Shift_Recover and  or else
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
end Debug;
--  Local Variables:
--  wisi-disable-face: t
--  End:
