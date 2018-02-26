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
with AUnit.Test_Filters;
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
with Test_Character_Literal;
with Test_Follow;
with Test_LR_Expecting;
with Test_McKenzie_Recover;
with Test_Skip_To;
with Test_Statement_Actions;
with Test_Wisi_Suite;
with Trivial_Productions_Test;
with WisiToken.Syntax_Trees.Test;
with WisiToken.Syntax_Trees.Branched.Test;
procedure Test_All_Harness
is
   use Ada.Command_Line;

   --  command line arguments: [test_name [routine_name [trace_level [mckenzie_trace_level]]]]
   --
   --  test_name, routine_name can be '' to set trace for all routines.

   Trace_Parse    : constant Integer := (if Argument_Count > 2 then Integer'Value (Argument (3)) else 0);
   Trace_McKenzie : constant Integer := (if Argument_Count > 3 then Integer'Value (Argument (4)) else 0);

   function New_Name_Filter (Test_Name, Routine_Name : in String) return AUnit.Test_Filters.Test_Filter_Access
   is
      use AUnit.Test_Filters;
   begin
      if Test_Name = "" and Routine_Name = "" then
         return null;
      else
         return Filter : constant Test_Filter_Access := new Name_Filter do
            if Test_Name = "" then
               Set_Name (Name_Filter (Filter.all), Routine_Name);
            elsif Routine_Name = "" then
               Set_Name (Name_Filter (Filter.all), Test_Name);
            else
               Set_Name (Name_Filter (Filter.all), Test_Name & " : " & Routine_Name);
            end if;
         end return;
      end if;
   end New_Name_Filter;

   Options : constant AUnit.Options.AUnit_Options :=
     (Global_Timer     => False,
      Test_Case_Timer  => False,
      Report_Successes => True,
      Filter           =>
        (case Argument_Count is
         when 0      => null,
         when 1      => New_Name_Filter (Argument (1), ""),
         when others => New_Name_Filter (Argument (1), Argument (2))));

   Suite    : constant Access_Test_Suite := Test_Wisi_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;
begin
   WisiToken.Trace_Parse    := Trace_Parse;
   WisiToken.Trace_McKenzie := Trace_McKenzie;

   --  Test cases; test package alphabetical order, unless otherwise noted.

   Add_Test (Suite, new Association_Grammar_Test.Test_Case (Debug => False));
   Add_Test (Suite, new Compare_Goto_Transitions.Test_Case (Debug => False));
   Add_Test (Suite, new Dragon_4_43_LR1_Test.Test_Case (Debug => 0));
   Add_Test (Suite, new Dragon_4_45_LALR_Test.Test_Case (Debug => 0));
   Add_Test (Suite, new Grune_9_30.Test_Case (Debug => 0));
   Add_Test (Suite, new Name_Grammar_Test.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Accept_State.Test_Case (Debug => 0));
   Add_Test (Suite, new Test_Ada_Lite.Test_Case);
   Add_Test (Suite, new Test_Character_Literal.Test_Case);
   Add_Test (Suite, new Test_Follow.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR_Expecting.Test_Case (Debug => 0));
   Add_Test (Suite, new Test_McKenzie_Recover.Test_Case (Cost_Limit => Natural'Last));
   Add_Test (Suite, new Test_Skip_To.Test_Case (Debug => 0));
   Add_Test (Suite, new Test_Statement_Actions.Test_Case (Debug => 0));
   Add_Test (Suite, new Trivial_Productions_Test.Test_Case (Debug => 0));
   Add_Test (Suite, new WisiToken.Syntax_Trees.Test.Test_Case);
   Add_Test (Suite, new WisiToken.Syntax_Trees.Branched.Test.Test_Case);

   --  end test cases

   Run (Suite, Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_All_Harness;
