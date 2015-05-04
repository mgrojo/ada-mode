with Ada.Strings.Unbounded;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada_Grammar_2;     use Ada_Grammar_2;
with ada_grammar_2_dfa; use ada_grammar_2_dfa;
with GNAT.OS_Lib;
with OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
procedure Run_Ada_Parser_2
is
   procedure Put_Usage
   is begin
      Put_Line ("usage: run_ada_parser <count> <file> [trace]");
      Put_Line ("parse <file> <count> times, for profiling the parser");
      Put_Line ("if [trace] is present, output parser trace");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String; -- for error message
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Count          : Integer;
   File           : GNAT.OS_Lib.File_Descriptor;
   Counted_Feeder : OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Handle;
   Feeder         : OpenToken.Text_Feeder.Text_Feeder_Ptr;
   Parser         : LALR_Parsers.Instance := Create_Parser (Terminate_Same_State => True);

   procedure Use_File (File_Name : in String)
   is
      use GNAT.OS_Lib;
      File_Size : Integer;

   begin
      Run_Ada_Parser_2.File_Name := +File_Name;

      File := Open_Read (File_Name, Text);
      --  Mode Text normalizes CR/LF to LF

      Feeder := OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Create (File);

      Counted_Feeder := OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Handle (Feeder);

      File_Size := Integer (File_Length (File));

      ada_grammar_2_dfa.Set_Buffer_Size (File_Size);

      Counted_Feeder.Reset (File_Size);
      Parser.Set_Text_Feeder (Feeder);

   exception
   when Name_Error =>
      Put_Line (File_Name & " cannot be opened");
      raise OpenToken.User_Error;
   end Use_File;

begin
   if Argument_Count = 2 then
      Count := Integer'Value (Argument (1));
      Use_File (Argument (2));

   elsif Argument_Count = 3 then
      Count := Integer'Value (Argument (1));
      Use_File (Argument (2));

      OpenToken.Trace_Parse := 1;
      aflex_debug           := True;

   else
      Put_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   for I in 1 .. Count loop
      LALR_Parsers.Parse (Parser);
      GNAT.OS_Lib.Lseek (File, 0, GNAT.OS_Lib.Seek_Set);
      Counted_Feeder.Reset (Integer (GNAT.OS_Lib.File_Length (File)));
      Parser.Reset;
   end loop;

exception
when E : OpenToken.Parse_Error | OpenToken.Syntax_Error =>
   Put_Line (-File_Name & ":" & Ada.Exceptions.Exception_Message (E));
   Set_Exit_Status (Failure);

when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Set_Exit_Status (Failure);

end Run_Ada_Parser_2;
