--  Abstract:
--
--  see spec
--
--  Copyright (C) 2015, 2017 - 2022 Stephe Leake
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
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Text_IO_Trace;
procedure Gen_LR_Parser_No_Recover_Run
is
   procedure Put_Usage
   is
      use Ada.Command_Line;
   begin
      Put (Command_Name);
      for I in 1 .. Argument_Count loop
         Put (' ' & Argument (I));
      end loop;
      New_Line;

      Put_Line (" usage: [<options>] filename");
      Put_Line (Ada.Command_Line.Command_Name & " usage: [-v <integer>] filename");
      Put_Line ("  parse input file, executing grammar actions");
      Put_Line ("  --verbosity <string> : trace options");
      Put_Line ("  -no-state-numbers : no state numbers in parse trace; for test_lr1_parallel");
   end Put_Usage;

   File_Name : Ada.Strings.Unbounded.Unbounded_String;
   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (Item : in Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   Trace    : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File : Ada.Text_IO.File_Type;

   procedure Parse
   is
      Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser;
   begin
      WisiToken.Parse.LR.Parser_No_Recover.New_Parser
        (Parser, Create_Lexer (Trace'Unchecked_Access), Create_Parse_Table, Create_In_Parse_Actions,
         Create_Post_Parse_Actions, User_Data => null);

      Parser.Tree.Lexer.Reset_With_File (-File_Name);
      Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);
      Parser.Parse (Log_File);

      --  No user data, so no point in calling Execute_Actions

      Parser.Put_Errors;

   exception
   when WisiToken.Syntax_Error =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Parser.Put_Errors;

   when E : WisiToken.Parse_Error =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line (Ada.Exceptions.Exception_Message (E));

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

         if Argument (Arg_Next) = "--verbosity" then
            Arg_Next  := Arg_Next + 1;
            WisiToken.Enable_Trace (Argument (Arg_Next));
            Arg_Next  := Arg_Next + 1;

         elsif Argument (Arg_Next) = "-no-state-numbers" then
            Arg_Next := Arg_Next + 1;

            WisiToken.Trace_Parse_No_State_Numbers := True;

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
end Gen_LR_Parser_No_Recover_Run;
