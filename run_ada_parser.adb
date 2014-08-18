with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada_Grammar;
with OpenToken.Text_Feeder.Text_IO;
procedure Run_Ada_Parser
is
   File     : File_Type;
   Feeder   : OpenToken.Text_Feeder.Text_Feeder_Ptr;
   Analyzer : Ada_Grammar.Analyzers.Instance := Ada_Grammar.Analyzers.Initialize (Ada_Grammar.Syntax, Feeder);
begin
   Open (File, In_File, Argument (1));
   Set_Input (File);
   Feeder := OpenToken.Text_Feeder.Text_IO.Create (Standard_Input);
   Ada_Grammar.Analyzers.Set_Text_Feeder (Analyzer, Feeder);

   Ada_Grammar.LALR_Parsers.Parse (Ada_Grammar.Parser);

exception
when E : others =>
   if Is_Open (File) then
      Put_Line (Count'Image (Line (File)) & ": " & Ada.Exceptions.Exception_Message (E));
   else
      Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   end if;

end Run_Ada_Parser;
