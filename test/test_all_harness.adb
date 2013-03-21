--  Abstract :
--
--  Run all OpenToken AUnit tests; see Makefile for other tests.
--
--  Copyright (C) 2009, 2010, 2012, 2013 Stephen Leake.  All Rights Reserved.
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
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Analyzer_Lookahead_Test;
with OpenToken.Recognizer.Bracketed_Comment.Test;
with OpenToken.Recognizer.CSV_Field.Test;
with Test_Accept_Index;
with Test_Backtrack;
with Test_LR0_Kernels;
with Test_LR1_Lookahead_Closure;
with Test_LR_Expecting;
with Test_List_Actions;
with Test_List_Stack;
with Test_Selection_Actions;
with Test_Sequence_Actions;
with Test_Statement_Actions;
with Test_Token_Identifier_Real_String;
with Test_Wisi_Suite;
with Trivial_Productions_Test;
procedure Test_All_Harness
is
   Suite    : constant Access_Test_Suite := Test_Wisi_Suite;
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   Result   : AUnit.Test_Results.Result;
   Status   : AUnit.Status;
begin
   --  Test cases; test package alphabetical order, unless otherwise noted.

   Add_Test (Suite, new Analyzer_Lookahead_Test.Test_Case (Debug => False));
   Add_Test (Suite, new OpenToken.Recognizer.Bracketed_Comment.Test.Test_Case);
   Add_Test (Suite, new OpenToken.Recognizer.CSV_Field.Test.Test_Case);
   Add_Test (Suite, new Test_Accept_Index.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Backtrack.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR0_Kernels.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR1_Lookahead_Closure.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR_Expecting.Test_Case (Debug => False));
   Add_Test (Suite, new Test_List_Actions.Test_Case (Debug => False));
   Add_Test (Suite, new Test_List_Stack.Test_Case);
   Add_Test (Suite, new Test_Selection_Actions.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Sequence_Actions.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Statement_Actions.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Token_Identifier_Real_String.Test_Case (Debug => False));
   Add_Test (Suite, new Trivial_Productions_Test.Test_Case (Debug => False));

   --  end test cases

   Run (Suite, AUnit.Options.Default_Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

end Test_All_Harness;
