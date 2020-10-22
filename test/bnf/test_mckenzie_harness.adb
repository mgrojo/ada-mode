--  Abstract :
--
--  Run Test_McKenzie_Recover
--
--  Copyright (C) 2019 - 2020 Stephen Leake.  All Rights Reserved.
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
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line; use Ada.Command_Line;
with Test_McKenzie_Recover;
with WisiToken.BNF;
with WisiToken;
procedure Test_McKenzie_Harness
is
   Usage : constant String :=
     --  command line arguments (all optional, order matters):
     "[LALR | LR1] routine_name trace_config";
   --  1           2            3
   --  trace_config is passed to Wisitoken.Enable_Trace
   --
   --  routine_name can be '' to set trace for all routines.

   use all type WisiToken.BNF.Generate_Algorithm;

   Alg : constant WisiToken.BNF.Generate_Algorithm :=
     (if Argument_Count >= 1 then WisiToken.BNF.Generate_Algorithm'Value (Argument (1)) else None);

   Force_High_Cost_Solutions : constant Boolean := False;
   --    (if Argument_Count >= 7 then 0 /= Integer'Value (Argument (7)) else False);

   Force_Full_Explore : constant Boolean := False;
   --    (if Argument_Count >= 8 then 0 /= Integer'Value (Argument (8)) else False);

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
   if Argument_Count >= 2 then
      Filter.Set_Name ("test_mckenzie_recover.adb " & Alg'Image & " : " & Argument (2));
   end if;

   if Argument_Count = 3 then
      WisiToken.Enable_Trace (Argument (3));
   end if;

   if Argument_Count > 3 then
      raise Constraint_Error with Usage;
   end if;

   Filter.Verbose := WisiToken.Trace_Tests > 0;

   if Alg in None | LALR then
      Add_Test
        (Suite,
         Test_Case_Access'
           (new Test_McKenzie_Recover.Test_Case
              (WisiToken.BNF.LALR, Force_Full_Explore, Force_High_Cost_Solutions)));
   end if;

   if Alg in None | LR1 then
      Add_Test
        (Suite,
         Test_Case_Access'
           (new Test_McKenzie_Recover.Test_Case
              (WisiToken.BNF.LR1, Force_Full_Explore, Force_High_Cost_Solutions)));
   end if;

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_McKenzie_Harness;
