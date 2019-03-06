--  Used to throw Constraint_Error in wisi.adb Delete_Token
begin
               Update_Prev := True;
               Prev_Token.Non_Grammar.Append (Deleted_Token.Non_Grammar);

               if Deleted_Token.First_Trailing_Comment_Line /= Invalid_Line_Number then
                  if Prev_Token.First_Trailing_Comment_Line = Invalid_Line_Number then
                     if Deleted_Token.First then
                        Prev_Token.First_Trailing_Comment_Line := Deleted_Token.First_Indent_Line;
                     else
                        Prev_Token.First_Trailing_Comment_Line := Deleted_Token.First_Trailing_Comment_Line;
                     end if;
                  end if;
                  Prev_Token.Last_Trailing_Comment_Line  := Deleted_Token.Last_Trailing_Comment_Line;
               end if;
            end;
         end if;
      end if;

      loop
         exit when Data.Terminals (Next_Token_Index).Deleted = False;
         Next_Token_Index := Next_Token_Index + 1;
exit when Next_Token_Index = Data.Terminals.Last_Index;
end loop;
