procedure Ada_Mode.Recover_21
is
begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count < 1 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Source_File_Name  := +Ada.Command_Line.Argument (1);
      Post_Parse_Action := Wisi_Runtime.Post_Parse_Action_Type'Value (Ada.Command_Line.Argument (2));
      Arg               := 3;

      loop
         exit when Arg > Argument_Count;

         if A then
         elsif Argument (Arg) = "--lang_params" then
            Lang_Params := +Argument (Arg + 1);
            Arg := Arg + 2;

         elsif Argument (Arg) = "--repeat_count" then
            Repeat_Count := Integer'Value (Argument (Arg + 1));
            Arg := Arg + 2;

         else
            Put_Line ("unrecognized option: '" & Argument (Arg) & "'");
            Put_Usage;
            return;
         end if;
      end loop;
   end;

end Ada_Mode.Recover_21;
