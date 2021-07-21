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
with Ada_Annex_P_Process_Actions; use Ada_Annex_P_Process_Actions;
with Ada_Annex_P_Process_LALR_Main;
with WisiToken.AUnit;
with WisiToken.Parse.LR.McKenzie_Recover.Ada;
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

   type Token_Enum_ID_Array is array (Positive range <>) of Token_Enum_ID;

   procedure Check
     (Label : in String;
      Expected : in Token_Enum_ID_Array)
   --  Compare Expected to Tree.shared_stream element roots.
   is
      use WisiToken.AUnit;
      Ref : Stream_Node_Ref := Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => False);
   begin
      for I in Expected'Range loop
         Check (Label & I'Image, Tree.ID (Ref.Node), +Expected (I), Ada_Annex_P_Process_Actions.Descriptor);
         Tree.Stream_Next (Ref, Rooted => True);
      end loop;
   end Check;

   ----------
   --  Test procedures

   procedure Test_Left_Breakdown_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      --  Test deleting a leading empty nonterm.
      Ref : Stream_Node_Ref := Parse_Text ("for A in B loop end loop;");
   begin
      Tree.Left_Breakdown (Ref);

      if Trace_Tests > Outline then
         Put_Line ("left_breakdown:");
         Put_Line (Tree.Image (Ref.Stream, Stack => True, Input => True, Shared => True, Children => True));
      end if;

      Check ("1",
             (Wisi_SOI_ID, FOR_ID, iterator_specification_ID, LOOP_ID, sequence_of_statements_ID, END_ID, LOOP_ID,
              identifier_opt_ID, SEMICOLON_ID, Wisi_EOI_ID));
   end Test_Left_Breakdown_1;

   procedure Test_Right_Breakdown_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      --  Test deleting a trailing empty nonterm.
      Ref : Stream_Node_Ref := Parse_Text ("package A is end;");
   begin
      --  Expose package_specification, which has a trailing empty nonterm
      Tree.Right_Breakdown (Ref);
      Check ("SEMICOLON", Tree.ID (Ref.Node), +SEMICOLON_ID, Ada_Annex_P_Process_Actions.Descriptor);

      Tree.Stream_Prev (Ref, Rooted => True);
      Check
        ("package_specification", Tree.ID (Ref.Node), +package_specification_ID,
         Ada_Annex_P_Process_Actions.Descriptor);

      Tree.Right_Breakdown (Ref);

      if Trace_Tests > Outline then
         Put_Line ("right_breakdown:");
         Put_Line (Tree.Image (Ref.Stream, Stack => True, Input => True, Shared => True, Children => True));
      end if;

      Check ("1", (Wisi_SOI_ID, PACKAGE_ID, name_ID, IS_ID, END_ID, SEMICOLON_ID, Wisi_EOI_ID));
   end Test_Right_Breakdown_1;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Left_Breakdown_1'Access, "Test_Left_Breakdown_1");
      Register_Routine (T, Test_Right_Breakdown_1'Access, "Test_Right_Breakdown_1");
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
         Ada_Annex_P_Process_LALR_Main.Create_Lexer,
         Ada_Annex_P_Process_LALR_Main.Create_Parse_Table,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada.Language_Fixes'Access,
         Language_Matching_Begin_Tokens => WisiToken.Parse.LR.McKenzie_Recover.Ada.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada.String_ID_Set'Access,
         User_Data                      => User_Data'Access);
   end Set_Up_Case;

end Test_Syntax_Trees;
--  Local Variables:
--  ada-case-strict: nil
--  End:
