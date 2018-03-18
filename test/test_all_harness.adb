--  Abstract :
--
--  Run all WisiToken AUnit tests; see Makefile for other tests.
--
--  Copyright (C) 2009, 2010, 2012 - 2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO;
with Association_Grammar_Test;
with Compare_Goto_Transitions;
with Dragon_4_43_LR1_Test;
with Dragon_4_45_LALR_Test;
with GNAT.Traceback.Symbolic;
with Grune_9_30;
with Name_Grammar_Test;
with Test_Accept_State;
with Test_Ada_Lite;
with Test_Ada_Lite_Terminal_Sequence;
with Test_Character_Literal;
with Test_Follow;
with Test_LR_Expecting_Terminal_Sequence;
with Test_McKenzie_Recover;
with Test_Skip_To;
with Test_Wisi_Suite;
with Trivial_Productions_Test;
with WisiToken.Syntax_Trees.Branched.Test;
procedure Test_All_Harness
is
   --  command line arguments: [<verbose> [test_name [routine_name [trace_parse [trace_mckenzie [cost_limit]]]]]
   --  <verbose> is 1 | 0; 1 lists each enabled test/routine name before running it
   --
   --  test_name, routine_name can be '' to set trace for all routines.

   Filter : aliased AUnit.Test_Filters.Verbose.Filter;

   Cost_Limit : Natural;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           => Filter'Unchecked_Access);

   Suite    : constant Access_Test_Suite := Test_Wisi_Suite;
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
      Cost_Limit               := (if Argument_Count > 5 then Natural'Value (Argument (6)) else Natural'Last);
   end;

   --  Test cases; test package alphabetical order, unless otherwise noted.

   Add_Test (Suite, new Association_Grammar_Test.Test_Case);
   Add_Test (Suite, new Compare_Goto_Transitions.Test_Case (Debug => False));
   Add_Test (Suite, new Dragon_4_43_LR1_Test.Test_Case (Debug => 0));
   Add_Test (Suite, new Dragon_4_45_LALR_Test.Test_Case);
   Add_Test (Suite, new Grune_9_30.Test_Case);
   Add_Test (Suite, new Name_Grammar_Test.Test_Case);
   Add_Test (Suite, new Test_Accept_State.Test_Case);
   Add_Test (Suite, new Test_Ada_Lite.Test_Case);
   Add_Test (Suite, new Test_Ada_Lite_Terminal_Sequence.Test_Case);
   Add_Test (Suite, new Test_Character_Literal.Test_Case);
   Add_Test (Suite, new Test_Follow.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR_Expecting_Terminal_Sequence.Test_Case);
   Add_Test (Suite, new Test_McKenzie_Recover.Test_Case (Cost_Limit));
   Add_Test (Suite, new Test_Skip_To.Test_Case);
   Add_Test (Suite, new Trivial_Productions_Test.Test_Case);
   Add_Test (Suite, new WisiToken.Syntax_Trees.Branched.Test.Test_Case);

   --  end test cases

   Run (Suite, Options, Result, Status);

   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_All_Harness;
