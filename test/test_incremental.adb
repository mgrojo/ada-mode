--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2021 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LR1_T1_Main;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Parse.LR.Parser_Lists;
with WisiToken.Syntax_Trees.AUnit_Public;
with WisiToken.Text_IO_Trace;
package body Test_Incremental is
   Trace     : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File  : Ada.Text_IO.File_Type;
   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Parser : WisiToken.Parse.LR.Parser.Parser;

   procedure Parse_Text
     (Initial   : in String;
      Edit_At   : in Integer;
      Delete    : in String;
      Insert    : in String;
      Edit_2_At : in Integer := 0;
      Delete_2  : in String  := "";
      Insert_2  : in String  := "")
   with Pre => Edit_2_At = 0 or Edit_2_At > Edit_At
   is
      use Ada.Text_IO;
      use AUnit.Checks;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use WisiToken.Parse;
      use all type WisiToken.Base_Buffer_Pos;
      use all type Ada.Containers.Count_Type;

      Saved_Trace_Parse : constant Integer := WisiToken.Trace_Parse;

      Edited : String (Initial'First .. Initial'Last + Insert'Length + Insert_2'Length) := (others => ' ');

      Edited_Last : Integer := Initial'Last;

      Edited_Tree_Batch : WisiToken.Syntax_Trees.Tree;

      Edits : KMN_Lists.List;

      KMN_Next_Bytes : Integer := Initial'First;
      KMN_Next_Chars : Integer := Initial'First;

      procedure Edit_Text
        (Edit_At : in Integer;
         Delete  : in String;
         Insert  : in String)
      is begin
         if Delete'Length > 0 then
            if Initial (Edit_At .. Edit_At + Delete'Length - 1) /= Delete then
               AUnit.Assertions.Assert (False, "invalid delete");
            end if;
            Edited (Edit_At .. Edited'Last - Delete'Length) := Edited (Edit_At + Delete'Length .. Edited'Last);
            Edited (Edited'Last - Delete'Length + 1 .. Edited'Last) := (others => ' ');
         end if;

         if Insert'Length > 0 then
            Edited (Edit_At + Insert'Length .. Edited'Last) := Edited (Edit_At .. Edited'Last - Insert'Length);
            Edited (Edit_At .. Edit_At + Insert'Length - 1) := Insert;
         end if;

         Edited_Last := Edited_Last - Delete'Length + Insert'Length;
      end Edit_Text;

      procedure To_KMN
        (Edit_At : in Integer;
         Delete  : in String;
         Insert  : in String)
      is
         use WisiToken;

         Edit_1 : constant KMN :=
           (Stable_Bytes   => Base_Buffer_Pos (Edit_At - KMN_Next_Bytes),
            Stable_Chars   => Base_Buffer_Pos (Edit_At - KMN_Next_Bytes), --  FIXME: test utf-8
            Deleted_Bytes  => Delete'Length,
            Deleted_Chars  => Delete'Length,
            Inserted_Bytes => Insert'Length,
            Inserted_Chars => Insert'Length);
      begin
         Edits.Append (Edit_1);

         KMN_Next_Bytes := @ + Integer (Edit_1.Stable_Bytes + Edit_1.Deleted_Bytes);
         KMN_Next_Chars := @ + Integer (Edit_1.Stable_Chars + Edit_1.Deleted_Chars);
      end To_KMN;

      procedure Last_KMN
      is
         use WisiToken;
      begin
         if KMN_Next_Bytes <= Initial'Last then
            Edits.Append
              ((Stable_Bytes   => Base_Buffer_Pos (Initial'Last - KMN_Next_Bytes + 1),
                Stable_Chars   => Base_Buffer_Pos (Initial'Last - KMN_Next_Chars + 1),
                Deleted_Bytes  => 0,
                Deleted_Chars  => 0,
                Inserted_Bytes => 0,
                Inserted_Chars => 0));
         end if;
         --  EOI is also in a "stable region", but that is handled specially in
         --  Edit_Tree.
      end Last_KMN;

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
      Edited (Initial'First .. Initial'Last) := Initial;

      if Edit_At in Initial'Range then
         if Edit_2_At in Initial'Range then
            Edit_Text (Edit_2_At, Delete_2, Insert_2);
         end if;
         Edit_Text (Edit_At, Delete, Insert);
      end if;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("initial source: '" & Initial & "'");
         Ada.Text_IO.Put_Line ("edited source : '" & Edited (Edited'First .. Edited_Last) & "'");
      end if;

      --  Batch parse of Edited
      if WisiToken.Trace_Tests < 2 then
         --  Don't trace parse in batch parser.
         WisiToken.Trace_Parse := 0;
      end if;

      Parser.Tree.Lexer.Reset_With_String (Edited (Edited'First .. Edited_Last));

      Parser.Parse (Log_File);

      Parser.Tree.Clear_Parse_Streams;
      Parser.Tree.Copy_Tree (Edited_Tree_Batch, User_Data'Access);

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Put_Tree ("edited source batch parse", Edited_Tree_Batch);
      end if;

      --  Batch parse of Initial
      Parser.Tree.Lexer.Reset_With_String (Initial);

      Parser.Parse (Log_File);

      Parser.Tree.Clear_Parse_Streams;

      WisiToken.Trace_Parse := Saved_Trace_Parse;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         New_Line;
         Put_Tree ("initial source batch parse result", Parser.Tree);
      end if;

      --  Prepare for incremental parse
      Parser.Tree.Lexer.Reset_With_String (Edited (Edited'First .. Edited_Last));

      if Edit_At in Initial'Range then
         To_KMN (Edit_At, Delete, Insert);
         if Edit_2_At in Initial'Range then
            To_KMN (Edit_2_At, Delete_2, Insert_2);
         end if;
         Last_KMN;
      end if;

      if Edits.Length > 0 then
         if WisiToken.Trace_Tests > WisiToken.Outline then
            Put_Line ("KMN_List:" & Image (Edits));
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
              (WisiToken.Base_Buffer_Pos (Edited'First), WisiToken.Base_Buffer_Pos (Edited_Last)),
            Edited_Text_Char_Region  =>
              (WisiToken.Base_Buffer_Pos (Edited'First), WisiToken.Base_Buffer_Pos (Edited_Last)));
      end if;

      if WisiToken.Trace_Parse + WisiToken.Trace_Incremental_Parse > WisiToken.Outline then
         New_Line;
         Put_Line ("incremental parse:");
      end if;

      Parser.Parse (Log_File, Edits);
      Parser.Tree.Clear_Parse_Streams;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         New_Line;
         Put_Tree ("incremental parse result", Parser.Tree);
      end if;

      Check ("1", Parser.Tree, Edited_Tree_Batch, Shared_Stream => False);
   exception
   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Tests > WisiToken.Outline then
         New_Line;
         Put_Tree ("(syntax_error) incremental parse result", Parser.Tree);
      end if;

      Check ("syntax_error", True, False);

   when E : WisiToken.Parse_Error =>
      Check ("parse_error: " & Ada.Exceptions.Exception_Message (E), True, False);
   end Parse_Text;

   ----------
   --  Test procedures

   procedure No_Change (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial => "A := B + C;",
         --          1        |10
         Edit_At => 0,
         Delete  => "",
         Insert  => "");
   end No_Change;

   procedure Edit_Comment (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial => "A := B + C; --  A comment",
         --          1        |10       |20
         Edit_At => 19,
         Delete  => "comment",
         Insert  => "cool explanation");
      --             |19        |30

   end Edit_Comment;

   procedure Edit_Comment_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial   =>
           "package Test is" & ASCII.LF &
           --        |10       |16
           "   --  A comment" & ASCII.LF &
           --  |20       |30
           "   function Bar return Integer;" & ASCII.LF &
           --  |34   |40       |50       |60
           "end Test;",
           --  |65
         Edit_At   => 19,
         Delete    => " ",
         Insert    => "",
         Edit_2_At => 36,
         Delete_2  => " ",
         Insert_2  => "");

   end Edit_Comment_2;

   procedure Edit_Leading_Non_Grammar (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      Parse_Text
        (Initial   =>
           "--  Leading comment" & ASCII.LF &
           --        |10           |20
           "package Test is" & ASCII.LF &
           "   --  A comment" & ASCII.LF &
           "   function Bar return Integer;" & ASCII.LF &
           "end Test;",
         Edit_At   => 3,
         Delete    => " ",
         Insert    => "");

   end Edit_Leading_Non_Grammar;

   procedure Edit_Code_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert at start of initial text
      Parse_Text
        (Initial => "A := --  comment 1" & ASCII.LF & "B + C; -- comment 2",
         --          1        |10     |18              |20       |30     |38
         Edit_At => 1,
         Delete  => "",
         Insert  => "A_");
      --             |1
   end Edit_Code_1;

   procedure Edit_Code_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert, delete in middle
      Parse_Text
        (Initial => "A := --  comment 1" & ASCII.LF & "B + C; -- comment 2",
         --          1        |10     |18              |20       |30     |38
         Edit_At => 20,
         Delete  => "B",
         Insert  => "A_1");
      --             |20
   end Edit_Code_2;

   procedure Edit_Code_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert, delete at last grammar token
      Parse_Text
        (Initial => "A := --  comment 1" & ASCII.LF & "B + C;",
         --          1        |10     |18              |20
         Edit_At => 25,
         Delete  => ";",
         Insert  => "_1;");
      --             |25
   end Edit_Code_3;

   procedure Edit_Code_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert, delete at different places, insert first. Insert
      --  potentially extends a preceding token, delete modifies
      --  a following token.
      Parse_Text
        (Initial => "A := --  comment 1" & ASCII.LF & "Bd + Cc;",
         --          |1       |10     |18              |20
         Edit_At => 5,
         Delete  => "",
         Insert  => "1 + ",
         --          |5

         Edit_2_At => 20,
         Delete_2  => "Bd + C",
         Insert_2  => "");

      --  Edited: "A :=1 +  --  comment 1" & ASCII.LF & "c;",
      --           |1       |10       |20                |24
   end Edit_Code_4;

   procedure Edit_Code_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert, delete at different places, delete first. Delete part of
      --  preceding token, insert extends preceding token.
      Parse_Text
        (Initial => "A_23 := --  comment 1" & ASCII.LF & "B + C;",
         --          1        |10       |20               |23  |27
         Edit_At => 4,
         Delete  => "3",
         Insert  => "",

         Edit_2_At => 24,
         Delete_2  => "",
         Insert_2  => "_2");
   end Edit_Code_5;

   procedure Edit_Code_6 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete after insert, modifying token
      Parse_Text
        (Initial => "A := B + C;",
         --          |1   |6  |10
         Edit_At => 2,
         Delete  => "",
         Insert  => "_1",
         --          |2

         Edit_2_At => 7,
         Delete_2  => " + ",
         Insert_2  => "");

      --  Edited: "A_1 := BC;",
      --           |1     |8
   end Edit_Code_6;

   procedure Edit_Code_7 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete whitespace after insert, _not_ modifying token
      Parse_Text
        (Initial => "A := B +  C;",
         --          |1       |10
         Edit_At => 2,
         Delete  => "",
         Insert  => "_1",
         --          |2

         Edit_2_At => 9,
         Delete_2  => " ",
         Insert_2  => "");
   end Edit_Code_7;

   procedure Edit_Code_8 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete/insert same length text
      Parse_Text
        (Initial => "A := B + Cab;",
         --          |1       |10
         Edit_At => 10,
         Delete  => "Cab",
         Insert  => "Cad");
   end Edit_Code_8;

   procedure Delete_New_Line (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete a newline
      Parse_Text
        (Initial => "A := B + C;" & ASCII.LF & "D;",
         --          |1       |10
         Edit_At => 12,
         Delete  => "" & ASCII.LF,
         Insert  => "");
   end Delete_New_Line;

   procedure Insert_New_Line (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert a newline
      Parse_Text
        (Initial => "A := B + C; D;",
         --          |1       |10
         Edit_At => 12,
         Delete  => " ",
         Insert  => "" & ASCII.LF);
   end Insert_New_Line;

   procedure Names (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test that Names are preserved by incremental edits
      Parse_Text
        (Initial => "procedure Name is begin null; end Name;",
         --          |1       |10       |20
         Edit_At => 25,
         Delete  => "null",
         Insert  => "A := A + 1");

      declare
         use WisiToken.Syntax_Trees;
         use Ada_Lite_Actions;
         use AUnit.Checks;
         Tree : WisiToken.Syntax_Trees.Tree renames Parser.Tree;
         Begin_Name_Node : constant Valid_Node_Access := Tree.Find_Descendant (Tree.Root, +subprogram_specification_ID);
         End_Name_Node   : constant Valid_Node_Access := Tree.Find_Descendant (Tree.Root, +name_opt_ID);
      begin
         Check
           ("name",
            Tree.Lexer.Buffer_Text (Tree.Name (Begin_Name_Node)),
            Tree.Lexer.Buffer_Text (Tree.Name (End_Name_Node)));
      end;
   end Names;

   procedure Recover_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Full parse uses error recovery to place missing return type.
      --  Incremental parse fixes the error.
      Parse_Text
        (Initial =>
           "function Func_1 (A : Integer) return " & ASCII.LF & -- missing "Integer"
           --        |10       |20       |30
           "is begin return 1; end;",
         Edit_At => 38,
         Delete  => "",
         Insert  => "Integer");
   end Recover_1;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Change'Access, "No_Change");
      Register_Routine (T, Edit_Comment'Access, "Edit_Comment");
      Register_Routine (T, Edit_Comment_2'Access, "Edit_Comment_2");
      Register_Routine (T, Edit_Leading_Non_Grammar'Access, "Edit_Leading_Non_Grammar");
      Register_Routine (T, Edit_Code_1'Access, "Edit_Code_1");
      Register_Routine (T, Edit_Code_2'Access, "Edit_Code_2");
      Register_Routine (T, Edit_Code_3'Access, "Edit_Code_3");
      Register_Routine (T, Edit_Code_4'Access, "Edit_Code_4");
      Register_Routine (T, Edit_Code_5'Access, "Edit_Code_5");
      Register_Routine (T, Edit_Code_6'Access, "Edit_Code_6");
      Register_Routine (T, Edit_Code_7'Access, "Edit_Code_7");
      Register_Routine (T, Edit_Code_8'Access, "Edit_Code_8");
      Register_Routine (T, Delete_New_Line'Access, "Delete_New_Line");
      Register_Routine (T, Insert_New_Line'Access, "Insert_New_Line");
      Register_Routine (T, Names'Access, "Names");
      Register_Routine (T, Recover_1'Access, "Recover_1");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_incremental.adb");
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
