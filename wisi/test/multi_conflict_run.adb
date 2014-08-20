with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with OpenToken.Text_Feeder.Text_IO;
with Multi_Conflict;
procedure Multi_Conflict_Run
is
   File : File_Type;
begin
   Open (File, In_File, Argument (1));
   Set_Input (File);
   Multi_Conflict.LALR_Parsers.Set_Text_Feeder
     (Multi_Conflict.Parser, OpenToken.Text_Feeder.Text_IO.Create (Standard_Input));

   Multi_Conflict.LALR_Parsers.Parse (Multi_Conflict.Parser);

exception
when E : others =>
   if Is_Open (File) then
      Put_Line (Count'Image (Line (File)) & ": " & Ada.Exceptions.Exception_Message (E));
   else
      Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   end if;

end Multi_Conflict_Run;
