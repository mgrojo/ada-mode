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
with Skip_To_Grammar_LALR_Main;
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

   Ada_Lite_Parser : WisiToken.Parse.LR.Parser.Parser;
   Skip_To_Parser  : WisiToken.Parse.LR.Parser.Parser;

   function Parse_Text (Parser : in out WisiToken.Parse.LR.Parser.Parser; Text : in String) return Stream_Node_Ref
   --  return wisitoken_accept in parse_stream.
   is begin
      Parser.Tree.Lexer.Reset_With_String (Text);

      Parser.Parse (Log_File);
      Parser.Tree.Start_Edit;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Put_Line ("shared stream:");
         Put_Line (Parser.Tree.Image (Parser.Tree.Shared_Stream, Input => True, Shared => True, Children => True));
         New_Line;
      end if;

      return Parser.Tree.Stream_First (Parser.Tree.Shared_Stream, Skip_SOI => True);
   end Parse_Text;

   ----------
   --  Test procedures

   procedure Left_Breakdown_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use Ada_Lite_Actions;

      --  The block_statement has a leading empty nonterm that must be
      --  deleted.
      Ref : Stream_Node_Ref := Parse_Text (Ada_Lite_Parser, "begin A := B; end;");
      Tree : WisiToken.Syntax_Trees.Tree renames Ada_Lite_Parser.Tree;
   begin
      Tree.Left_Breakdown (Ref, User_Data'Access);

      if Trace_Tests > Outline then
         Put_Line ("left_breakdown:");
         Put_Line (Tree.Image (Ref.Stream, Stack => True, Input => True, Shared => True, Children => True));
      end if;
      Check ("1 el", Tree.ID (Ref.Stream, Ref.Element), +BEGIN_ID);
      Check ("1 node", Tree.ID (Ref.Node), +BEGIN_ID);
      Check_Address ("1 el = node", Tree.Get_Node (Ref.Stream, Ref.Element), Ref.Node);
   end Left_Breakdown_1;

   procedure Find_New_Line_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
      use Ada_Lite_Actions;

      --  The requested line is started by a comment_new_line
      Text : constant String :=
        "A; -- comment" & ASCII.LF & "B;";
      --  2       |10                 |15
      Begin_Char_Pos : WisiToken.Buffer_Pos;

   begin
      Ada_Lite_Parser.Tree.Lexer.Reset_With_String (Text);

      Ada_Lite_Parser.Parse (Log_File);

      declare
         Tree : WisiToken.Syntax_Trees.Tree renames Ada_Lite_Parser.Tree;
         Node : constant WisiToken.Syntax_Trees.Node_Access := Tree.Find_New_Line (2, Begin_Char_Pos);
      begin
         Check ("1 node", Tree.ID (Node), +SEMICOLON_ID);
         Check ("1 begin_char_pos", Begin_Char_Pos, 15);
      end;
   end Find_New_Line_1;

   procedure Byte_Region_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
      use Ada_Lite_Actions;

      --  Byte_Region of an empty nonterm.
      Text : constant String :=
        "procedure A is begin null; end A;";
      --  2       |10                 |15

   begin
      Ada_Lite_Parser.Tree.Lexer.Reset_With_String (Text);

      Ada_Lite_Parser.Parse (Log_File);

      declare
         Tree : WisiToken.Syntax_Trees.Tree renames Ada_Lite_Parser.Tree;
         Node : constant WisiToken.Syntax_Trees.Node_Access := Tree.Find_Descendant
           (Tree.Root, +parameter_profile_opt_ID);
      begin
         Check ("1 byte_region", Tree.Byte_Region (Node), (12, 11));
      end;
   end Byte_Region_1;

   procedure Line_At_Byte_Pos_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      Text : constant String :=
        "procedure A" & ASCII.LF & "is begin" & ASCII.LF & "   null;" & ASCII.LF & " end A;";
      --          |10               |13         |21                     |30
   begin
      Ada_Lite_Parser.Tree.Lexer.Reset_With_String (Text);

      Ada_Lite_Parser.Parse (Log_File);

      Check ("1", Ada_Lite_Parser.Tree.Line_At_Byte_Pos (13), 2);
   end Line_At_Byte_Pos_1;

   procedure Line_At_Byte_Pos_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      Text : constant String :=
        "%{Preamble line 1" & ASCII.LF & "  Preamble line 2 }%" & ASCII.LF & "%keyword PERCENT;";
      --          |10         |18           |21      |30          |39
   begin
      Skip_To_Parser.Tree.Lexer.Reset_With_String (Text);

      Skip_To_Parser.Parse (Log_File);

      if Trace_Tests > Outline then
         Skip_To_Parser.Tree.Print_Tree (Trace, Non_Grammar => True);
      end if;

      Check ("1", Skip_To_Parser.Tree.Line_At_Byte_Pos (13), 1);
      Check ("2", Skip_To_Parser.Tree.Line_At_Byte_Pos (21), 2);
      Check ("3", Skip_To_Parser.Tree.Line_At_Byte_Pos (41), 3);
   end Line_At_Byte_Pos_2;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Left_Breakdown_1'Access, "Left_Breakdown_1");
      Register_Routine (T, Find_New_Line_1'Access, "Find_New_Line_1");
      Register_Routine (T, Byte_Region_1'Access, "Byte_Region_1");
      Register_Routine (T, Line_At_Byte_Pos_1'Access, "Line_At_Byte_Pos_1");
      Register_Routine (T, Line_At_Byte_Pos_2'Access, "Line_At_Byte_Pos_2");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_syntax_trees.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      --  Run before all tests in register
      WisiToken.Parse.LR.Parser.New_Parser
        (Ada_Lite_Parser,
         Trace'Access,
         Ada_Lite_LR1_T1_Main.Create_Lexer,
         Ada_Lite_LR1_T1_Main.Create_Parse_Table
           (Text_Rep_File_Name          => "ada_lite_lr1_t1_re2c_parse_table.txt"),
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      WisiToken.Parse.LR.Parser.New_Parser
        (Skip_To_Parser,
         Trace'Access,
         Skip_To_Grammar_LALR_Main.Create_Lexer,
         Skip_To_Grammar_LALR_Main.Create_Parse_Table ("skip_to_grammar_lalr_parse_table.txt"),
         Language_Fixes                 => null,
         Language_Matching_Begin_Tokens => null,
         Language_String_ID_Set         => null,
         User_Data                      => null);
   end Set_Up_Case;

end Test_Syntax_Trees;
--  Local Variables:
--  ada-case-strict: nil
--  End:
