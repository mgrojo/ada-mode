--  Abstract :
--
--  Run one OpenToken AUnit test
--
--  Copyright (C) 2009, 2010, 2012 - 2014, 2017 Stephen Leake.  All Rights Reserved.
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

with Ada.Command_Line;
with AUnit.Reporter.Text;
with AUnit.Options;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Parser_Lists_Test;
procedure Test_One_Harness
is
   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

   Debug : constant Integer :=
     (if Ada.Command_Line.Argument_Count > 0 then Integer'Value (Ada.Command_Line.Argument (1))
      else 0);
   --  pragma Unreferenced (Debug);
begin
   Add_Test (Suite, new Parser_Lists_Test.Test_Case (Debug > 0));

   Run (Suite, AUnit.Options.Default_Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_One_Harness;
