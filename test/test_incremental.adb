--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with AUnit.Checks;
with Ada.Containers;
with Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LR1_T1_Main;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Parse.LR;
with WisiToken.Syntax_Trees.AUnit_Public;
with WisiToken.Text_IO_Trace;
package body Test_Incremental is
   use Ada_Lite_Actions;

   Trace     : aliased WisiToken.Text_IO_Trace.Trace;
   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Parser : WisiToken.Parse.LR.Parser.Parser (Ada_Lite_Actions.Descriptor'Access);

   procedure Parse_Text
     (Initial              : in String;
      Edit_At              : in Integer;
      Delete               : in String;
      Insert               : in String;
      Compare_Node_Numbers : in Boolean)
   is
      use Ada.Text_IO;
      use AUnit.Checks;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use WisiToken.Parse;
      use all type WisiToken.Base_Buffer_Pos;

      Initial_First  : Integer := Initial'First;
      Initial_Last   : Integer := Initial'First - 1;

      Edited : String (Initial'First .. Initial'Last + Insert'Length);
      Edited_First  : Integer := Edited'First;
      Edited_Last   : Integer := Edited'First - 1;

      Edited_Tree_Batch : WisiToken.Syntax_Trees.Tree (Ada_Lite_Actions.Descriptor'Access);

      Edits : KMN_Lists.List;

      procedure Check_Tree (Label : in String; Tree : in out WisiToken.Syntax_Trees.Tree)
      is
         use all type Ada.Containers.Count_Type;
         Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
      begin
         Tree.Validate_Tree (User_Data, Parser.Line_Begin_Char_Pos, Label, Error_Reported);

         Standard.AUnit.Assertions.Assert (Error_Reported.Count = 0, Label & ": error in validate_tree");
      end Check_Tree;

      procedure Put_Tree (Label : in String; Tree : in WisiToken.Syntax_Trees.Tree)
      is begin
         Put_Line (Label & ":");
         Parser.Put_Errors;

         Put_Line (" ... streams:");
         Tree.Print_Streams (Non_Grammar => True);

         if WisiToken.Trace_Tests > WisiToken.Detail then
            Put_Line (" ... tree:");
            Tree.Print_Tree (Non_Grammar => True);
         end if;
      end Put_Tree;

   begin
      --  Create Edited string
      if Edit_At in Initial'Range then
         Edited_Last  := Edit_At - 1;
         Initial_Last := Edited_Last;

         Edited (Edited_First .. Edited_Last) := Initial (Initial_First .. Initial_Last);

         Initial_Last := Edit_At + Delete'Length - 1;

         if Initial (Edit_At .. Initial_Last) /= Delete then
            AUnit.Assertions.Assert (False, "invalid delete");
         end if;
      end if;

      if Edit_At in Initial'Range then
         Edited_First := Edited_Last + 1;
         Edited_Last := Edited_Last + Insert'Length;

         Edited (Edited_First .. Edited_Last) := Insert;
      end if;

      Initial_First := Initial_Last + 1;
      Initial_Last := Initial'Last;
      if Initial_First <= Initial_Last then
         Edited_First := Edited_Last + 1;
         Edited_Last  := Edited_Last + Initial_Last - Initial_First + 1;

         Edited (Edited_First .. Edited_Last) := Initial (Initial_First .. Initial_Last);
      end if;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("initial source: '" & Initial & "'");
         Ada.Text_IO.Put_Line ("edited source : '" & Edited (Edited'First .. Edited_Last) & "'");
      end if;

      --  Batch parse of Edited
      Parser.Lexer.Reset_With_String (Edited (Edited'First .. Edited_Last));

      Parser.Parse;

      Check_Tree ("edited source batch parse 0", Parser.Tree);

      Parser.Tree.Copy_Tree (Edited_Tree_Batch, User_Data'Access);

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Put_Tree ("edited source batch parse", Edited_Tree_Batch);
      end if;

      Check_Tree ("edited source batch parse 1", Edited_Tree_Batch);

      --  Batch parse of Initial
      Parser.Lexer.Reset_With_String (Initial);

      Parser.Parse;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         New_Line;
         Put_Tree ("initial source batch parse", Parser.Tree);
      end if;

      Check_Tree ("initial source batch parse", Parser.Tree);

      --  Prepare for incremental parse
      Parser.Lexer.Reset_With_String (Edited (Edited'First .. Edited_Last));

      if Edit_At in Initial'Range then
         declare
            use WisiToken;

            Edit_1 : constant KMN :=
              (Stable_Bytes   => Base_Buffer_Pos (Edit_At - Initial'First),
               Stable_Chars   => Base_Buffer_Pos (Edit_At - Initial'First), --  FIXME: test utf-8
               Deleted_Bytes  => Delete'Length,
               Deleted_Chars  => Delete'Length,
               Inserted_Bytes => Insert'Length,
               Inserted_Chars => Insert'Length);
         begin
            Edits.Append (Edit_1);

            if Edit_1.Stable_Bytes + Edit_1.Deleted_Bytes < Base_Buffer_Pos (Initial'Last) then
               Edits.Append
                 ((Stable_Bytes   => Base_Buffer_Pos (Initial'Last) -
                     Edit_1.Stable_Bytes + Edit_1.Deleted_Bytes,
                   Stable_Chars   => Base_Buffer_Pos (Initial'Last) -
                     Edit_1.Stable_Bytes + Edit_1.Deleted_Bytes,
                   Deleted_Bytes  => 0,
                   Deleted_Chars  => 0,
                   Inserted_Bytes => 0,
                   Inserted_Chars => 0));
            end if;
         end;

         --  EOI is also in a "stable region", but that is handled specially in
         --  Edit_Tree.
      end if;

      Validate_KMN
        (List => Edits,
         Stable_Byte_First        => WisiToken.Base_Buffer_Pos (Initial'First),
         Stable_Char_First        => WisiToken.Base_Buffer_Pos (Initial'First),
         Initial_Text_Byte_Region =>
           (WisiToken.Base_Buffer_Pos (Initial'First), WisiToken.Base_Buffer_Pos (Initial'Last)),
         Initial_Text_Char_Region =>
           (WisiToken.Base_Buffer_Pos (Initial'First), WisiToken.Base_Buffer_Pos (Initial'Last)),
         --  FIXME: test utf-8
         Edited_Text_Byte_Region  =>
           (WisiToken.Base_Buffer_Pos (Edited'First), WisiToken.Base_Buffer_Pos (Edited'Last)),
         Edited_Text_Char_Region  =>
           (WisiToken.Base_Buffer_Pos (Edited'First), WisiToken.Base_Buffer_Pos (Edited'Last)));

      WisiToken.Parse.Edit_Tree (Parser, Edits);

      if WisiToken.Trace_Tests > WisiToken.Outline then
         New_Line;
         Put_Tree ("edited tree", Parser.Tree);
      end if;

      Check_Tree ("edited tree 1", Parser.Tree);

      --  FIXME: Parser.Parse (Incremental => True);

      Check ("1", Parser.Tree, Edited_Tree_Batch, Compare_Node_Numbers);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Parse > WisiToken.Outline then
         Parser.Put_Errors;
      end if;

      Check ("exception", True, False);
   end Parse_Text;

   ----------
   --  Test procedures

   procedure No_Change (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial              => "A := B + C;",
         --                       1        |10
         Edit_At              => 0,
         Delete               => "",
         Insert               => "",
         Compare_Node_Numbers => True);
   end No_Change;

   procedure Edit_Comment (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial              => "A := B + C; --  A comment",
         --                       1        |10       |20
         Edit_At              => 19,
         Delete               => "comment",
         Insert               => "cool explanation",
         --                       |19        |30
         Compare_Node_Numbers => True);
   end Edit_Comment;

   procedure Edit_Code_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial              => "A := --  comment 1" & ASCII.LF & "B + C; -- comment 2",
         --                       1        |10     |18              |20       |30     |38
         Edit_At              => 1,
         Delete               => "",
         Insert               => "A_",
         --                       |1
         Compare_Node_Numbers => True);
   end Edit_Code_1;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Change'Access, "No_Change");
      Register_Routine (T, Edit_Comment'Access, "Edit_Comment");
      Register_Routine (T, Edit_Code_1'Access, "Edit_Code_1");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_incremental.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      --  Run before all tests in register
      Ada_Lite_LR1_T1_Main.Create_Parser
        (Parser,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         Trace                          => Trace'Access,
         User_Data                      => User_Data'Access,
         Text_Rep_File_Name             => "ada_lite_lr1_t1_re2c_parse_table.txt");
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      null;
   end Set_Up;

end Test_Incremental;
--  Local Variables:
--  ada-case-strict: nil
--  End:
