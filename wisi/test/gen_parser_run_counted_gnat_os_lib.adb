--  Abstract:
--
--  see spec
--
--  Copyright (C) 2015, 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with FastToken.Text_Feeder.Counted_GNAT_OS_Lib;
procedure Gen_Parser_Run_Counted_GNAT_OS_Lib
is
   procedure Put_Usage
   is begin
      Put_Line ("usage: *_run [-v <integer>] filename");
      Put_Line ("  parse input file, executing grammar actions");
      Put_Line ("  -v : output trace of states while parsing");
      --  Always run both LALR and LR1 parses.
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String; -- for error message
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   File_Length : Integer;
   LALR_Parser : LR_Parser.Instance;
   LR1_Parser  : LR_Parser.Instance;

   procedure Use_File (File_Name : in String)
   is
      use GNAT.OS_Lib;
   begin
      Gen_Parser_Run_Counted_GNAT_OS_Lib.File_Name := +File_Name;

      declare
         File : constant File_Descriptor := Open_Read (File_Name, Text);
         --  Mode Text normalizes CR/LF to LF

         Feeder : constant FastToken.Text_Feeder.Text_Feeder_Ptr :=
           FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Create (File);

         Counted_Feeder : FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance renames
           FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (Feeder.all);

      begin
         File_Length := Integer (GNAT.OS_Lib.File_Length (File));
         Counted_Feeder.Reset (File_Length + 1); -- +1 for EOF
         LALR_Parser := Create_Parser (FastToken.LALR, Text_Feeder => Feeder);
         LR1_Parser  := Create_Parser (FastToken.LR1, Text_Feeder => Feeder);
      end;
   exception
   when Name_Error =>
      Put_Line (File_Name & " cannot be opened");
      raise FastToken.User_Error;
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
            FastToken.Trace_Parse := Integer'Value (Argument (2));

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

   begin
      Put_Line ("LALR_Parser parse:");
      LALR_Parser.Parse;
   exception
   when E : FastToken.Parse_Error | FastToken.Syntax_Error =>
      Put_Line (Ada.Directories.Simple_Name (-File_Name) & ":" & Ada.Exceptions.Exception_Message (E));
   end;

   declare
      use GNAT.OS_Lib;

      Counted_Feeder : FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance renames
        FastToken.Text_Feeder.Counted_GNAT_OS_Lib.Instance (LR1_Parser.Lexer.Feeder.all);
   begin
      Counted_Feeder.Reset (File_Length + 1); -- +1 for EOF
   end;
   LR1_Parser.Lexer.Reset (1024);
   New_Line;

   begin
      Put_Line ("LR1_Parser parse:");
      LR1_Parser.Parse;
   exception
   when E : FastToken.Parse_Error | FastToken.Syntax_Error =>
      Put_Line (Ada.Directories.Simple_Name (-File_Name) & ":" & Ada.Exceptions.Exception_Message (E));
   end;

exception
when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Parser_Run_Counted_GNAT_OS_Lib;
