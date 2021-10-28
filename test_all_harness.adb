--  Abstract :
--
--  Run tests.
--
--  Copyright (C) 2020, 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Test_Filters.Verbose;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with Test_Edit_Source;
with WisiToken;
procedure Test_All_Harness
is
   Usage : constant String :=
     --  command line arguments (all optional, order matters):
     "test_name routine_name trace_config";
   --  1         2            3
   --  trace_config is passed to Wisitoken.Enable_Trace
   --
   --  test_name, routine_name can be '' to set trace for all routines.

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
   declare
      use Ada.Command_Line;
   begin
      Filter.Verbose := Argument_Count > 0 and then Argument (1) = "1";

      case Argument_Count is
      when 0 =>
         null;

      when 1 =>
         Filter.Test_Name := To_Unbounded_String (Argument (1));

      when 2 | 3 =>
         Filter.Test_Name    := To_Unbounded_String (Argument (1));
         Filter.Routine_Name := To_Unbounded_String (Argument (2));

         if Argument_Count = 3 then
            WisiToken.Enable_Trace (Argument (3));
         end if;

      when others =>
         raise Constraint_Error with Usage;
      end case;
   end;

   Filter.Verbose := WisiToken.Trace_Tests > 0;

   --  Test cases; test package alphabetical order, unless otherwise noted.

   Add_Test (Suite, Test_Case_Access'(new Test_Edit_Source.Test_Case));
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
end Test_All_Harness;
