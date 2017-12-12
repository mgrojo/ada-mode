--  Abstract :
--
--  Run all AUnit tests for SAL.
--
--  Copyright (C) 2003 - 2009, 2012, 2015, 2016, 2017 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 2, or (at your option) any
--  later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
--  General Public License for more details. You should have received a
--  copy of the GNU General Public License distributed with SAL; see
--  file COPYING. If not, write to the Free Software Foundation, 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with SAL.CSV.Test;
with SAL.File_Names.Test;
with SAL.Time_Conversions.Test;
with Test.Config_Files.All_Suite;
with Test_Bounded_Definite_Queues;
with Test_Definite_Doubly_Linked_Lists;
with Test_Gen_Images;
with Test_Min_Heap_Binary;
with Test_Min_Heap_Fibonacci;
with Test_Network_Order;
with Test_Randomize_Lists;
with Test_Red_Black_Trees;
with Test_Stacks;
with Test_Stats;
procedure Test_All_Harness
is
   Suite    : constant Access_Test_Suite := new Test_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;

begin
   --  This is first because it's a suite.
   Add_Test (Suite, Test.Config_Files.All_Suite);


   Add_Test (Suite, new SAL.CSV.Test.Test_Case);
   Add_Test (Suite, new SAL.File_Names.Test.Test_Case);
   Add_Test (Suite, new SAL.Time_Conversions.Test.Test_Case);
   Add_Test (Suite, new Test_Gen_Images.Test_Case);
   Add_Test (Suite, new Test_Bounded_Definite_Queues.Test_Case);
   Add_Test (Suite, new Test_Definite_Doubly_Linked_Lists.Test_Case);
   Add_Test (Suite, new Test_Min_Heap_Binary.Test_Case);
   Add_Test (Suite, new Test_Min_Heap_Fibonacci.Test_Case);
   Add_Test (Suite, new Test_Network_Order.Test_Case);
   Add_Test (Suite, new Test_Randomize_Lists.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Red_Black_Trees.Test_Case);
   Add_Test (Suite, new Test_Stacks.Test_Case);
   Add_Test (Suite, new Test_Stats.Test_Case);

   Run (Suite, AUnit.Options.Default_Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_All_Harness;
