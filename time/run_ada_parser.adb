with Ada.Strings.Unbounded;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada_Grammar_Process;     use Ada_Grammar_Process;
with ada_grammar_process_dfa; use ada_grammar_process_dfa;
with GNAT.OS_Lib;
with FastToken.Text_Feeder.Counted_GNAT_OS_Lib;
procedure Run_Ada_Parser
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
   File_Size      : Integer;
   Feeder         : FastToken.Text_Feeder.Text_Feeder_Ptr;
   Counted_Feeder : FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Handle;
   Parser         : LALR_Parser.Instance;

   procedure Use_File (File_Name : in String)
   is
      use GNAT.OS_Lib;

   begin
      Run_Ada_Parser.File_Name := +File_Name;

      File := Open_Read (File_Name, Text);
      --  Mode Text normalizes CR/LF to LF

      Feeder         := FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Create (File);
      Counted_Feeder := FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Handle (Feeder);
      File_Size      := Integer (File_Length (File));
      Counted_Feeder.Reset (File_Size);

      Parser := Create_Parser (Text_Feeder => Feeder, Buffer_Size => File_Size + 2);

   exception
   when Name_Error =>
      Put_Line (File_Name & " cannot be opened");
      raise FastToken.User_Error;
   end Use_File;

begin
   if Argument_Count = 2 then
      Count := Integer'Value (Argument (1));
      Use_File (Argument (2));

   elsif Argument_Count = 3 then
      Count := Integer'Value (Argument (1));
      Use_File (Argument (2));

      FastToken.Trace_Parse := 1;
      aflex_debug           := True;

   else
      Put_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   for I in 1 .. Count loop
      Parser.Parse;
      GNAT.OS_Lib.Lseek (File, 0, GNAT.OS_Lib.Seek_Set);
      Counted_Feeder.Reset (File_Size); -- FIXME; reset should do lseek?
      Parser.Reset (Buffer_Size => File_Size + 2);
   end loop;

exception
when E : FastToken.Parse_Error | FastToken.Syntax_Error =>
   Put_Line (-File_Name & ":" & Ada.Exceptions.Exception_Message (E));
   Set_Exit_Status (Failure);

when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Set_Exit_Status (Failure);

end Run_Ada_Parser;
