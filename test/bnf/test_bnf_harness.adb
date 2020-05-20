--  Abstract :
--
--  Run Test_BNF_Suite
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
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Environment_Variables;
with Test_BNF_Suite;
with WisiToken;
procedure Test_BNF_Harness
is
   --  command line arguments (all optional, order matters):
   --  <verbose> root_name trace_generate trace_parse
   --  1         2         3              4
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   EBNF_Only : constant Boolean :=
     Ada.Environment_Variables.Exists ("EBNF_ONLY") and then
     Ada.Environment_Variables.Value ("EBNF_ONLY") = "true";

   Suite    : constant Access_Test_Suite := Test_BNF_Suite (EBNF_Only);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

begin
   Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

   if Argument_Count > 1 then
      declare
         Test_Name : constant String := "bnf_wy_test.adb ";
         Root_Name : String renames Argument (2);
      begin
         Filter.Set_Name (Test_Name & Root_Name);
      end;
   end if;

   WisiToken.Trace_Generate_Table := (if Argument_Count >= 3 then Integer'Value (Argument (3)) else 0);
   WisiToken.Trace_Parse          := (if Argument_Count >= 4 then Integer'Value (Argument (4)) else 0);

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_BNF_Harness;
