procedure Ada_Mode.Recover_23
is begin
   for I of Trivia loop
      declare
         Token : Token_Data_Type renames TDH.Trivas (I).T;
         Line : constant Line_Number_Type := Line_Number_Type (Token.Sloc_Range.Start_Line);
      begin
         if Token.Kind = Ada_Comment then
            Token.Non_Grammar.Append
              ((ID    => +COMMENT_ID,
                Line  => Line,
                Col   => Ada.Text_IO.Count (Token.Sloc_Range.Start_Column),
                First => Line /= Token.Line));

         else
            raise WisiToken.Programmer_Error;
         end if;
      end;
   end loop;
end Ada_Mode.Recover_23;
