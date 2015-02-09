with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with OpenToken.Text_Feeder.Counted_GNAT_OS_Lib;
procedure Gen_Parser_Run_Counted_GNAT_OS_Lib
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
      use GNAT.OS_Lib;
   begin
      Gen_Parser_Run_Counted_GNAT_OS_Lib.File_Name := +File_Name;

      declare
         File : constant File_Descriptor := Open_Read (File_Name, Text);
         --  Mode Text normalizes CR/LF to LF

         Feeder : constant OpenToken.Text_Feeder.Text_Feeder_Ptr :=
           OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Create (File);

         Counted_Feeder : OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance renames
           OpenToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (Feeder.all);

      begin
         Counted_Feeder.Reset (Integer (File_Length (File)));
         The_Parser.Set_Text_Feeder (Feeder);
      end;
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
end Gen_Parser_Run_Counted_GNAT_OS_Lib;
