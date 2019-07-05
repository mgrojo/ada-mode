--  Abstract :
--
--  Run Test_McKenzie_Recover
--
--  Copyright (C) 2019 Stephen Leake.  All Rights Reserved.
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
with Test_McKenzie_Recover;
with WisiToken.BNF;
with WisiToken;
procedure Test_McKenzie_Harness
is
   --  command line arguments (all optional, order matters):
   --  <verbose> LALR|LR1 routine_name trace_parse trace_mckenzie trace_action
   --  1         2        3            4           5              6
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  routine_name can be '' to set trace for all routines.

   File_Name : constant String := "test_mckenzie_recover.adb ";

   use all type WisiToken.BNF.Generate_Algorithm;
   Alg : constant WisiToken.BNF.Generate_Algorithm :=
     (if Argument_Count >= 2 then WisiToken.BNF.Generate_Algorithm'Value (Argument (2)) else None);

   Force_High_Cost_Solutions : constant Boolean :=
     (if Argument_Count >= 7 then 0 /= Integer'Value (Argument (7)) else False);
   Force_Full_Explore : constant Boolean :=
     (if Argument_Count >= 8 then 0 /= Integer'Value (Argument (8)) else False);

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

begin
   Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

   case Argument_Count is
   when 0 | 1 =>
      null;

   when 2 =>
      Filter.Set_Name (File_Name & Argument (2)); -- test name only

   when others =>
      declare
         Test_Name    : constant String := File_Name & Argument (2);
         Routine_Name : String renames Argument (3);
      begin
         if Routine_Name = "" then
            Filter.Set_Name (Test_Name);
         else
            Filter.Set_Name (Test_Name & " : " & Routine_Name);
         end if;
      end;
   end case;

   WisiToken.Trace_Parse    := (if Argument_Count >= 4 then Integer'Value (Argument (4)) else 0);
   WisiToken.Trace_McKenzie := (if Argument_Count >= 5 then Integer'Value (Argument (5)) else 0);
   WisiToken.Trace_Action   := (if Argument_Count >= 6 then Integer'Value (Argument (6)) else 0);

   if Alg in None | LALR then
      Add_Test (Suite, new Test_McKenzie_Recover.Test_Case
                  (WisiToken.BNF.LALR, Force_Full_Explore, Force_High_Cost_Solutions));
   end if;

   if Alg in None | LR1 then
      Add_Test (Suite, new Test_McKenzie_Recover.Test_Case
                  (WisiToken.BNF.LR1, Force_Full_Explore, Force_High_Cost_Solutions));
   end if;

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_McKenzie_Harness;
