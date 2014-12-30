with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada_Grammar;
with OpenToken.Text_Feeder.Text_IO;
procedure Run_Ada_Parser
is
   procedure Put_Usage
   is begin
      Put_Line ("usage: run_ada_parser <count> <file>");
      Put_Line ("parse <file> <count> times, for profiling the parser");
   end Put_Usage;

   Count : Integer;
   File   : File_Type;
   Parser : Ada_Grammar.LALR_Parsers.Instance := Ada_Grammar.Create_Parser (Terminate_Same_State => True);
begin
   if Argument_Count = 2 then
      Count := Integer'Value (Argument (1));
      Open (File, In_File, Argument (2));
   else
      Put_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   Set_Input (File);
   Parser.Set_Text_Feeder (OpenToken.Text_Feeder.Text_IO.Create (Current_Input));

   for I in 1 .. Count loop
      Ada_Grammar.LALR_Parsers.Parse (Parser);
      Reset (File);
      Parser.Reset;
   end loop;

exception
when E : OpenToken.Parse_Error | OpenToken.Syntax_Error =>
   Put_Line (Name (File) & ":" & Ada.Exceptions.Exception_Message (E));
   Set_Exit_Status (Failure);

when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Set_Exit_Status (Failure);

end Run_Ada_Parser;
