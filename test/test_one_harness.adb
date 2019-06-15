--  Abstract :
--
--  Run one WisiToken AUnit test
--
--  Copyright (C) 2009, 2010, 2012 - 2014, 2017 - 2019 Stephen Leake.  All Rights Reserved.
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
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded;
with System.Multiprocessors;
with Test_McKenzie_Recover;
with WisiToken;
procedure Test_One_Harness
is
   --  command line arguments (all optional, order matters):
   --  <verbose> test_name routine_name trace_generate trace_parse trace_mckenzie trace_action task_count cost_limit
   --  1         2         3            4              5           6              7            8          9
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  routine_name can be '' to set trace or cost for all routines.

   Task_Count : constant System.Multiprocessors.CPU_Range :=
     (if Argument_Count >= 8 then System.Multiprocessors.CPU_Range'Value (Argument (8)) else 0);
   Cost_Limit : constant Natural := (if Argument_Count >= 9 then Natural'Value (Argument (9)) else Natural'Last);
   --  pragma Unreferenced (Task_Count, Cost_Limit);

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

   function "+" (Item : in String) return Ada.Strings.Unbounded.String_Access
   is begin
      return Ada.Strings.Unbounded.String_Access'(new String'(Item));
   end "+";
   pragma Unreferenced ("+");
begin
   Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

   case Argument_Count is
   when 0 | 1 =>
      null;

   when 2 =>
      Filter.Set_Name (Argument (2)); -- test name only

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

   WisiToken.Trace_Generate := (if Argument_Count >= 4 then Integer'Value (Argument (4)) else 0);
   WisiToken.Trace_Parse    := (if Argument_Count >= 5 then Integer'Value (Argument (5)) else 0);
   WisiToken.Trace_McKenzie := (if Argument_Count >= 6 then Integer'Value (Argument (6)) else 0);
   WisiToken.Trace_Action   := (if Argument_Count >= 7 then Integer'Value (Argument (7)) else 0);

   Add_Test (Suite, new Test_McKenzie_Recover.Test_Case (Task_Count, Cost_Limit));

   Run (Suite, Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_One_Harness;
