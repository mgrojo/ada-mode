--  Abstract :
--
--  Run all AUnit tests; see Makefile for other tests.
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with System.Multiprocessors;
with Test_Ada_Recover;
with Test_Gpr_Recover;
with WisiToken;
procedure Test_Harness
is
   --  command line arguments:
   --  <verbose> test_name routine_name trace_parse trace_mckenzie task_count cost_limit trace_action
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  test_name, routine_name can be '' to set trace for all routines.

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;

   Task_Count : System.Multiprocessors.CPU_Range;
   Cost_Limit : Natural;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;
begin
   declare
      use Ada.Command_Line;
   begin
      Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

      case Argument_Count is
      when 0 | 1 =>
         null;

      when 2 =>
         Filter.Set_Name (Argument (2));

      when others =>
         declare
            Test_Name    : String renames Argument (2);
            Routine_Name : String renames Argument (3);
         begin
            if Test_Name = "" then
               Filter.Set_Name (Routine_Name);
            elsif Routine_Name = "" then
               Filter.Set_Name (Test_Name);
            else
               Filter.Set_Name (Test_Name & " : " & Routine_Name);
            end if;
         end;
      end case;

      WisiToken.Trace_Parse    := (if Argument_Count > 3 then Integer'Value (Argument (4)) else 0);
      WisiToken.Trace_McKenzie := (if Argument_Count > 4 then Integer'Value (Argument (5)) else 0);
      Task_Count               := (if Argument_Count > 5
                                   then System.Multiprocessors.CPU_Range'Value (Argument (6)) else 0);
      Cost_Limit               := (if Argument_Count > 6 then Natural'Value (Argument (7)) else Natural'Last);
      WisiToken.Trace_Action   := (if Argument_Count > 7 then Integer'Value (Argument (8)) else 0);
      --  Trace_Action is used for verbosity in tests.
   end;

   --  Test cases; test package alphabetical order, unless otherwise noted.

   Add_Test (Suite, new Test_Ada_Recover.Test_Case (Task_Count, Cost_Limit));
   Add_Test (Suite, new Test_Gpr_Recover.Test_Case (Task_Count, Cost_Limit));

   --  end test cases

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

   case Status is
   when AUnit.Success =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   when AUnit.Failure =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end case;

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_Harness;
