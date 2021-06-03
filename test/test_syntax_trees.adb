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
with Ada_Lite_Actions;
with Ada_Lite_LR1_T1_Main;
with WisiToken.AUnit;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees.AUnit_Public;
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
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use Ada_Lite_Actions;

      --  The block_statement has a leading empty nonterm that must be
      --  deleted.
      Ref : Stream_Node_Ref := Parse_Text ("begin A := B; end;");
   begin
      Tree.Left_Breakdown (Ref);

      if Trace_Tests > Outline then
         Put_Line ("left_breakdown:");
         Put_Line (Tree.Image (Ref.Stream, Stack => True, Input => True, Shared => True, Children => True));
      end if;
      Check ("1 el", Tree.ID (Ref.Stream, Ref.Element), +BEGIN_ID);
      Check ("1 node", Tree.ID (Ref.Node), +BEGIN_ID);
      Check_Address ("1 el = node", Tree.Get_Node (Ref.Stream, Ref.Element), Ref.Node);
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
