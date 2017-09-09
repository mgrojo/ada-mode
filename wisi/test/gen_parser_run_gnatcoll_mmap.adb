--  Abstract:
--
--  see spec
--
--  Copyright (C) 2015, 2017 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Mmap;
with System;
procedure Gen_Parser_Run_GNATCOLL_Mmap
is
   procedure Put_Usage
   is begin
      Put_Line ("usage: *_run [-v <integer>] filename");
      Put_Line ("  parse input file, executing grammar actions");
      Put_Line ("  -v : output trace of states while parsing");
      --  Always run both LALR and LR1 parses.
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   LALR_Parser : WisiToken.Parser.LR.Parser.Instance := Create_Parser (WisiToken.LALR);
   LR1_Parser  : WisiToken.Parser.LR.Parser.Instance := Create_Parser (WisiToken.LR1);

   File   : GNATCOLL.Mmap.Mapped_File;
   Region : GNATCOLL.Mmap.Mapped_Region;

   procedure Parse (Algorithm : in WisiToken.Parser_Algorithm_Type)
   is
      use GNATCOLL.Mmap;
   begin
      File   := Open_Read (-File_Name);
      Region := Read (File);

      declare
         Buffer_Addr : constant System.Address := Data (Region).all'Address;
         Buffer      : String (1 .. Last (Region));
         for Buffer'Address use Buffer_Addr;
      begin
         case Algorithm is
         when WisiToken.LALR =>
            Put_Line ("LALR_Parser parse:");
            LALR_Parser.Lexer.Reset (Buffer);
            LALR_Parser.Parse;

         when WisiToken.LR1 =>
            Put_Line ("LR1_Parser parse:");
            LR1_Parser.Lexer.Reset (Buffer);
            LR1_Parser.Parse;
         end case;
      end;

      Free (Region);
      Close (File);

   exception
   when E : WisiToken.Parse_Error | WisiToken.Syntax_Error =>
      Put_Line (Ada.Directories.Simple_Name (-File_Name) & ":" & Ada.Exceptions.Exception_Message (E));
      Free (Region);
      Close (File);

   when Name_Error =>
      Put_Line (-File_Name & " cannot be opened");
      raise WisiToken.User_Error;
   end Parse;

begin
   declare
      use Ada.Command_Line;
   begin
      case Argument_Count is
      when 1 =>
         Gen_Parser_Run_GNATCOLL_Mmap.File_Name := +Argument (1);

      when 3 =>
         if Argument (1) = "-v" then
            WisiToken.Trace_Parse := Integer'Value (Argument (2));

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;

         Gen_Parser_Run_GNATCOLL_Mmap.File_Name := +Argument (1);

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

   Parse (WisiToken.LALR);
   Parse (WisiToken.LR1);

exception
when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Parser_Run_GNATCOLL_Mmap;
