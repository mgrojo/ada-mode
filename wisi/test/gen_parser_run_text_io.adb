with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with OpenToken.Text_Feeder.Text_IO;
procedure Gen_Parser_Run_Text_IO
is

   procedure Put_Usage
   is begin
      Put_Line ("usage: *_run [-v <integer>] filename");
      Put_Line ("  parse input file, executing grammar actions");
      Put_Line ("  -v : output trace of states while parsing");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String; -- for error message
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   The_Parser : LALR_Parsers.Instance := Create_Parser;

   procedure Use_File (File_Name : in String)
   is
      File : File_Type;
   begin
      Gen_Parser_Run_Text_IO.File_Name := +File_Name;

      Open (File, In_File, File_Name);
      Set_Input (File);
      --  Text_Feeder.Text_IO.Create must be called _after_ Set_Input
      LALR_Parsers.Set_Text_Feeder (The_Parser, OpenToken.Text_Feeder.Text_IO.Create (Current_Input));
   exception
   when Name_Error =>
      Put_Line (File_Name & " cannot be opened");
      raise OpenToken.User_Error;
   end Use_File;
begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 1 =>
         Use_File (Argument (1));

      when 3 =>
         if Argument (1) = "-v" then
            OpenToken.Trace_Parse := Integer'Value (Argument (2));

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

         Use_File (Argument (3));

      when others =>
         Set_Exit_Status (Failure);
         Put_Usage;
         return;
      end case;
   exception
   when others =>
      Set_Exit_Status (Failure);
      Put_Usage;
      return;
   end;

   LALR_Parsers.Parse (The_Parser);

exception
when E : OpenToken.Parse_Error | OpenToken.Syntax_Error =>
   Put_Line (Ada.Directories.Simple_Name (-File_Name) & ":" & Ada.Exceptions.Exception_Message (E));

when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Parser_Run_Text_IO;
