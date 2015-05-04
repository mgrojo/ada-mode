--  Abstract :
--
--  Run all OpenToken AUnit tests; see Makefile for other tests.
--
--  Copyright (C) 2009, 2010, 2012 - 2015 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
with AUnit.Options;
with AUnit.Reporter.Text;
with AUnit.Test_Results;
with AUnit.Test_Suites; use AUnit.Test_Suites;
with Analyzer_Buffer_Test;
with Counted_GNAT_OS_Lib_Test;
with GNAT.Traceback.Symbolic;
with OpenToken.Recognizer.Based_Integer_Real_Ada.Test;
with OpenToken.Recognizer.Bracketed_Comment.Test;
with OpenToken.Recognizer.CSV_Field.Test;
with Parser_Lists_Test;
with Test_Accept_Index;
with Test_Analyzer_Line_Column;
with Test_Empty_Productions_1;
with Test_Empty_Productions_4;
with Test_Empty_Productions_5;
with Test_Empty_Productions_6;
with Test_Empty_Productions_7;
with Test_Empty_Productions_8;
with Test_LR0_Kernels;
with Test_LR1_Lookahead_Closure;
with Test_LR_Expecting;
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

   Add_Test (Suite, new Analyzer_Buffer_Test.Test_Case);
   Add_Test (Suite, new Counted_GNAT_OS_Lib_Test.Test_Case);
   Add_Test (Suite, new OpenToken.Recognizer.Based_Integer_Real_Ada.Test.Test_Case);
   Add_Test (Suite, new OpenToken.Recognizer.Bracketed_Comment.Test.Test_Case);
   Add_Test (Suite, new OpenToken.Recognizer.CSV_Field.Test.Test_Case);
   Add_Test (Suite, new Parser_Lists_Test.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Accept_Index.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Analyzer_Line_Column.Test_Case);
   Add_Test (Suite, new Test_Empty_Productions_1.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Empty_Productions_4.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Empty_Productions_5.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Empty_Productions_6.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Empty_Productions_7.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Empty_Productions_8.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR0_Kernels.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR1_Lookahead_Closure.Test_Case (Debug => False));
   Add_Test (Suite, new Test_LR_Expecting.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Statement_Actions.Test_Case (Debug => False));
   Add_Test (Suite, new Test_Token_Identifier_Real_String.Test_Case (Debug => False));
   Add_Test (Suite, new Trivial_Productions_Test.Test_Case (Debug => False));

   --  end test cases

   Run (Suite, AUnit.Options.Default_Options, Result, Status);

   --  Provide command line option -v to set verbose mode
   AUnit.Reporter.Text.Report (Reporter, Result);

exception
when E : others =>
   Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Test_All_Harness;
