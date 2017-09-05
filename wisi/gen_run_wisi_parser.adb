--  Abstract :
--
--  Generic procedure to run a Wisi-generated parser.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.Parser.LR.Parser;
with WisiToken.Text_Feeder.Text_IO;
with WisiToken.Token_Region;
procedure Gen_Run_Wisi_Parser
is
   use all type Ada.Containers.Count_Type;

   --  Create the parser first, so we can access the default error
   --  correction parameters below.
   Parser : WisiToken.Parser.LR.Parser.Instance := Create_Parser (WisiToken.LALR);

   procedure Put_Usage
   is
      use all type WisiToken.Parser.LR.Parse_Table_Ptr;
   begin
      Put_Line ("run_" & Name & "_parser <file_name> [options]");
      Put_Line ("options:");
      Put_Line ("--verbosity n :");
      Put_Line ("   0 - only report parse errors");
      Put_Line ("   1 - shows each parser cycle, spawn/terminate parallel parsers, error recovery enter/exit");
      Put_Line ("   2 - add parse stack in each cycle, error recovery enqueue/check");
      Put_Line ("   3 - add pending semantic state operations, error recovery parse actions");
      Put_Line ("   4 - add lexer debug");
      Put_Line ("--cost_limit n   : set error recover cost limit; default" &
                  Integer'Image (Parser.Table.McKenzie.Cost_Limit));
      Put_Line ("--check_limit n  : set error recover token check limit; default" &
                  Integer'Image (Parser.Table.McKenzie.Check_Limit));
      Put_Line ("--repeat_count n : repeat parse count times, for profiling; default 1");
      New_Line;
   end Put_Usage;

   File_Name : Unbounded_String;

   Repeat_Count : Integer := 1;

   Arg : Integer := 2;
begin
   if Argument_Count < 1 then
      Put_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   File_Name := To_Unbounded_String (Argument (1));
   Parser.Lexer.Feeder := WisiToken.Text_Feeder.Text_IO.Create (To_String (File_Name));
   Aflex_Feeder := Parser.Lexer.Feeder;
   Parser.Lexer.Enable_Line_Numbers := True;

   loop
      exit when Arg + 1 > Argument_Count;

      if Argument (Arg) = "--verbosity" then
         WisiToken.Trace_Parse := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;

      elsif Argument (Arg) = "--cost_limit" then
         Parser.Table.McKenzie.Cost_Limit := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;

      elsif Argument (Arg) = "--check_limit" then
         Parser.Table.McKenzie.Check_Limit := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;

      elsif Argument (Arg) = "--repeat_count" then
         Repeat_Count := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;
      end if;
   end loop;

   if WisiToken.Trace_Parse > 3 then
      aflex_debug := True;
   end if;

   for I in 1 .. Repeat_Count loop
      Parser.Parse;
   end loop;

   WisiToken.Text_Feeder.Text_IO.Instance (Parser.Lexer.Feeder.all).Close;
   if Errors.Length > 0 then
      New_Line;
      Put_Line ("Errors:");
      WisiToken.Token_Region.Put (To_String (File_Name), Errors, Descriptor);
   end if;
exception
when E : WisiToken.Parse_Error | WisiToken.Syntax_Error =>
   New_Line;
   --  Exception message starts with ":<line>:<column>: "
   Put_Line (To_String (File_Name) & Ada.Exceptions.Exception_Message (E));

when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Run_Wisi_Parser;
