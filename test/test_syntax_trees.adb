--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2021 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lite_LR1_T1_Main;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
package body Test_Syntax_Trees is
   use WisiToken;
   use WisiToken.Syntax_Trees;

   Trace     : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File  : Ada.Text_IO.File_Type;
   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Parser : WisiToken.Parse.LR.Parser.Parser;
   Tree   : WisiToken.Syntax_Trees.Tree renames Parser.Tree;

   function Parse_Text (Text : in String) return Stream_Node_Ref
   --  return wisitoken_accept in parse_stream.
   is begin
      Parser.Tree.Lexer.Reset_With_String (Text);

      Parser.Parse (Log_File);
      Tree.Start_Edit;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Put_Line ("shared stream:");
         Put_Line (Tree.Image (Tree.Shared_Stream, Input => True, Shared => True, Children => True));
         New_Line;
      end if;

      return Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True);
   end Parse_Text;

   ----------
   --  Test procedures

   procedure Test_Left_Breakdown_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Ref : Stream_Node_Ref := Parse_Text ("begin A := B; end;");
   begin
      --  build a loop_statement to breakdown
      --  0210: (statement_3, (988 . 1035))
      --  0209: | (compound_statement_2, (988 . 1035))
      --  0208: | | (loop_statement_0, (988 . 1035))
      --  0199: | | | (label_opt_1)
      --  0091: | | | (iteration_scheme_2, (988 . 1014))
      --  0019: | | | | 1:(FOR, (1 . 3))
      --  0089: | | | | (iterator_specification_7, (992 . 1014))
      --  0020: | | | | | 2:(IDENTIFIER, (5 . 5))
      --  0021: | | | | | 3:(IN, (7 . 8))
      --  0088: | | | | | (name_0, (1005 . 1014))
      --  0087: | | | | | | (direct_name_0, (1005 . 1014))
      --  0022: | | | | | | | 4:(IDENTIFIER, (10 . 10))
      --  0023: | | | 4:(LOOP, (12 . 15))
      --  0206: | | | (sequence_of_statements_1)
      --  0205: | | | | (statement_statement_list_0)
      --  0204: | | | | | (statement_1)
      --  0203: | | | | | | (simple_statement_0)
      --  0202: | | | | | | | (null_statement_0)
      --  0200: | | | | | | | | -200:(NULL)
      --  0201: | | | | | | | | -201:(SEMICOLON)
      --  0052: | | | 5:(END, (17 . 19))
      --  0053: | | | 6:(LOOP, (20 . 23))
      --  0207: | | | (identifier_opt_1)
      --  0054: | | | 7:(SEMICOLON, (24 . 25))

      Tree.Left_Breakdown (Ref);

      if Trace_Tests > Outline then
         Put_Line ("left_breakdown:");
         Put_Line (Tree.Image (Ref.Stream, Stack => True, Input => True, Shared => True, Children => True));
      end if;
   end Test_Left_Breakdown_1;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Left_Breakdown_1'Access, "Test_Left_Breakdown_1");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_syntax_trees.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      --  Run before all tests in register
      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace'Access,
         Ada_Lite_LR1_T1_Main.Create_Lexer,
         Ada_Lite_LR1_T1_Main.Create_Parse_Table
           (Text_Rep_File_Name          => "ada_lite_lr1_t1_re2c_parse_table.txt"),
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         User_Data                      => User_Data'Access);
   end Set_Up_Case;

end Test_Syntax_Trees;
--  Local Variables:
--  ada-case-strict: nil
--  End:
