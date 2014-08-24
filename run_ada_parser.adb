with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada_Grammar;
with OpenToken.Text_Feeder.Text_IO;
procedure Run_Ada_Parser
is
   File   : File_Type;
   Parser : Ada_Grammar.LALR_Parsers.Instance := Ada_Grammar.Create_Parser;
begin
   if Argument (1) = "-v" then
      OpenToken.Trace_Parse := True;
      Open (File, In_File, Argument (2));
   else
      Open (File, In_File, Argument (1));
   end if;

   Set_Input (File);
   Ada_Grammar.LALR_Parsers.Set_Text_Feeder (Parser, OpenToken.Text_Feeder.Text_IO.Create (Current_Input));

   Ada_Grammar.LALR_Parsers.Parse (Parser);

exception
when E : others =>
   if Is_Open (File) then
      Put_Line (Count'Image (Line (File)) & ": " & Ada.Exceptions.Exception_Message (E));
   else
      Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   end if;

end Run_Ada_Parser;
