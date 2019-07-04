--  Abstract:
--
--  see spec
--
--  Copyright (C) 2015, 2017 - 2019 Stephe Leake
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
with System.Multiprocessors;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
procedure Gen_LR_Parser_Run
is
   procedure Put_Usage
   is begin
      Put_Line ("usage: *_run [options] filename");
      Put_Line ("  parse input file");
      Put_Line ("options:");
      Put_Line ("  -v <integer> : trace_parse");
      Put_Line ("  -m <integer> : trace_mckenzie");
      Put_Line ("  -t <integer> : mckenzie task count");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Trace : aliased WisiToken.Text_IO_Trace.Trace (Descriptor'Unrestricted_Access);
   --  Unrestricted_Access because can't make generic formal parameter aliased.

   Task_Count : System.Multiprocessors.CPU_Range := System.Multiprocessors.CPU_Range'Last;

   procedure Parse
   is
      use all type System.Multiprocessors.CPU_Range;
      Parser : WisiToken.Parse.LR.Parser.Parser;
   begin
      Create_Parser
        (Parser,
         Language_Fixes                 => Language_Fixes,
         Language_Matching_Begin_Tokens => Language_Matching_Begin_Tokens,
         Language_String_ID_Set         => Language_String_ID_Set,
         Trace                          => Trace'Unchecked_Access,
         User_Data                      => null);

      if Task_Count /= System.Multiprocessors.CPU_Range'Last then
         Parser.Table.McKenzie_Param.Task_Count := Task_Count;
      end if;

      Parser.Lexer.Reset_With_File (-File_Name);
      Parser.Parse;

      --  No user data, so no point in calling Execute_Actions

      Parser.Put_Errors;

   exception
   when WisiToken.Syntax_Error =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Parser.Put_Errors;

   when E : WisiToken.Parse_Error =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line (Ada.Directories.Simple_Name (-File_Name) & ":" & Ada.Exceptions.Exception_Message (E));

   when Name_Error =>
      Put_Line (-File_Name & " cannot be opened");
      raise WisiToken.User_Error;
   end Parse;

begin
   declare
      use Ada.Command_Line;
      Arg_Next : Integer := 1;
   begin
      loop
         exit when Argument (Arg_Next)(1) /= '-';

         if Argument (Arg_Next) = "-v" then
            Arg_Next  := Arg_Next + 1;
            WisiToken.Trace_Parse := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "-m" then
            Arg_Next  := Arg_Next + 1;
            WisiToken.Trace_McKenzie := Integer'Value (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "-t" then
            Arg_Next   := Arg_Next + 1;
            Task_Count := System.Multiprocessors.CPU_Range'Value (Argument (Arg_Next));
            Arg_Next   := Arg_Next + 1;

         else
            Set_Exit_Status (Failure);
            Put_Usage;
            return;
         end if;
      end loop;

      File_Name := +Argument (Arg_Next);
   exception
   when others =>
      Set_Exit_Status (Failure);
      Put_Usage;
      return;
   end;

   Parse;

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_LR_Parser_Run;
