--  Abstract :
--
--  Run Test_BNF_Suite
--
--  Copyright (C) 2019 - 2021, 2023 Stephen Leake.  All Rights Reserved.
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
with Test_BNF_Suite;
with WisiToken;
procedure Test_BNF_Harness
is
   --  command line arguments (all optional, order matters):
   --  root_name trace_config
   --  1         2
   --  trace_config is passed to Wisitoken.Enable_Trace

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   Suite    : constant Access_Test_Suite := Test_BNF_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

begin
   if Argument_Count >= 1 then
      if Argument (1)(1 .. 6) = "bnf_wy" then
         --  Allow bnf_wy_errors_test
         Filter.Test_Name := Ada.Strings.Unbounded.To_Unbounded_String (Argument (1));
      else
         Filter.Test_Name := Ada.Strings.Unbounded.To_Unbounded_String ("bnf_wy_test.adb " & Argument (1));
      end if;
   end if;

   if Argument_Count = 2 then
      WisiToken.Enable_Trace (Argument (2));
   end if;

   Filter.Verbose := WisiToken.Trace_Tests > 0;

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_BNF_Harness;
