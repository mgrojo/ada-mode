--  Abstract :
--
--  Generic procedure to run a standalone Wisi-generated parser.
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
with Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with WisiToken.Parser.LR.Parser;
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
      Put_Line ("--disable_recover : disable error recovery; default enabled");
      Put_Line ("--lexer_only : only run lexer, for profiling");
      Put_Line ("--repeat_count n : repeat parse count times, for profiling; default 1");
      Put_Line ("--pause : when repeating, prompt for <enter> after each parse; allows seeing memory leaks");
      New_Line;
   end Put_Usage;

   File_Name    : Unbounded_String;
   Lexer_Only   : Boolean := False;
   Repeat_Count : Integer := 1;
   Pause        : Boolean := False;

   Arg : Integer := 2;

   Start  : Ada.Real_Time.Time;
begin
   if Argument_Count < 1 then
      Put_Usage;
      Set_Exit_Status (Failure);
      return;
   end if;

   File_Name := To_Unbounded_String (Argument (1));

   loop
      exit when Arg > Argument_Count;

      if Argument (Arg) = "--verbosity" then
         WisiToken.Trace_Parse := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;

      elsif Argument (Arg) = "--cost_limit" then
         Parser.Table.McKenzie.Cost_Limit := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;

      elsif Argument (Arg) = "--check_limit" then
         Parser.Table.McKenzie.Check_Limit := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;

      elsif Argument (Arg) = "--disable_recover" then
         Parser.Enable_McKenzie_Recover := False;
         Arg := Arg + 1;

      elsif Argument (Arg) = "--lexer_only" then
         Lexer_Only := True;
         Arg := Arg + 1;

      elsif Argument (Arg) = "--pause" then
         Pause := True;
         Arg := Arg + 1;

      elsif Argument (Arg) = "--repeat_count" then
         Repeat_Count := Integer'Value (Argument (Arg + 1));
         Arg := Arg + 2;

      else
         Put_Line ("unrecognized option: '" & Argument (Arg) & "'");
         Put_Usage;
         return;
      end if;
   end loop;

   --  Do this after setting Trace_Parse so lexer verbosity is set
   Parser.Lexer.Reset_With_File (To_String (File_Name));
   Parser.Lexer.Enable_Line_Numbers := True;

   if Repeat_Count > 1 then
      Start := Ada.Real_Time.Clock;
   end if;

   for I in 1 .. Repeat_Count loop
      begin
         Parser.Lexer.Reset;
         if Lexer_Only then
            declare
               use WisiToken;
               ID : Token_ID := Invalid_Token_ID;
            begin
               Parser.Lexer.Reset;
               loop
                  exit when ID = Parser.Semantic_State.Trace.Descriptor.EOF_ID;
                  ID := Parser.Lexer.Find_Next;
               end loop;
            end;
         else
            Parser.Parse;
         end if;
      exception
      when WisiToken.Parse_Error | WisiToken.Syntax_Error =>
         New_Line;
         if I = 1 then
            Put_Line (To_String (File_Name) & ": exception Syntax_Error");
            if Errors.Length > 0 then
               New_Line;
               Put_Line ("Errors:");
               WisiToken.Token_Region.Put (To_String (File_Name), Errors, Descriptor);
            end if;
         end if;
      end;
      if Pause then
         Put_Line ("Enter to continue:");
         Flush (Standard_Output);
         declare
            Junk : constant String := Get_Line;
            pragma Unreferenced (Junk);
         begin
            null;
         end;
      end if;
   end loop;

   if Repeat_Count > 1 then
      declare
         use Ada.Real_Time;
         Finish : constant Time := Clock;
      begin
         Put_Line ("Total time:" & Duration'Image (To_Duration (Finish - Start)));
         Put_Line ("per iteration:" & Duration'Image (To_Duration ((Finish - Start) / Repeat_Count)));
      end;
   end if;

   if Errors.Length > 0 then
      New_Line;
      Put_Line ("Corrected Errors:");
      WisiToken.Token_Region.Put (To_String (File_Name), Errors, Descriptor);
   end if;

exception
when E : others =>
   New_Line;
   Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Gen_Run_Wisi_Parser;
