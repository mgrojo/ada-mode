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
with AUnit.Checks.Containers;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LR1_T1_Main;
with GNAT.Traceback.Symbolic;
with WisiToken.AUnit;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.Parser;
with WisiToken.Syntax_Trees.AUnit_Public;
with WisiToken.Text_IO_Trace;
with WisiToken.UTF_8;
package body Test_Incremental is
   Trace     : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File  : Ada.Text_IO.File_Type;
   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Incremental_Parser : WisiToken.Parse.LR.Parser.Parser;
   Full_Parser        : WisiToken.Parse.LR.Parser.Parser;

   Initial_Buffer : Ada.Strings.Unbounded.Unbounded_String;
   Edited_Buffer  : Ada.Strings.Unbounded.Unbounded_String;

   procedure Parse_Text
     (Initial     : in String;
      Edit_At     : in Integer;
      Delete      : in String;
      Insert      : in String;
      Edit_2_At   : in Integer                   := 0;
      Delete_2    : in String                    := "";
      Insert_2    : in String                    := "";
      Full_Errors : in Ada.Containers.Count_Type := 0;
      Incr_Errors : in Ada.Containers.Count_Type := 0;
      Label       : in String                    := "")
   with Pre => Edit_2_At = 0 or Edit_2_At > Edit_At
   --  If Initial is "", start from existing tree.
   is
      use Ada.Text_IO;
      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use WisiToken.Parse;
      use all type WisiToken.Base_Buffer_Pos;
      use all type Ada.Containers.Count_Type;

      Label_Dot : constant String :=
        (if Label = ""
         then ""
         else Label & ".");

      Edited_Source_Full_Parse_Tree : WisiToken.Syntax_Trees.Tree;

      Edits : KMN_Lists.List;

      KMN_Next_Bytes : Integer := 1;
      KMN_Next_Chars : Integer := 1;

      procedure Edit_Text
        (Edit_At : in Integer;
         Delete  : in String;
         Insert  : in String)
      is
         Edited : String (1 .. Length (Edited_Buffer) + Length (Initial_Buffer) + Insert'Length + Insert_2'Length) :=
           (others => ' ');
         Edited_Last : Integer := Length (Edited_Buffer);
      begin
         Edited (1 .. Length (Edited_Buffer)) := To_String (Edited_Buffer);

         if Delete'Length > 0 then
            if Initial (Edit_At .. Edit_At + Delete'Length - 1) /= Delete then
               AUnit.Assertions.Assert
                 (False, "invalid delete: '" & Delete & "' /= '" &
                    Initial (Edit_At .. Edit_At + Delete'Length - 1) & "'");
            end if;
            Edited (Edit_At .. Edited'Last - Delete'Length) := Edited (Edit_At + Delete'Length .. Edited'Last);
            Edited (Edited'Last - Delete'Length + 1 .. Edited'Last) := (others => ' ');
         end if;

         if Insert'Length > 0 then
            Edited (Edit_At + Insert'Length .. Edited'Last) := Edited (Edit_At .. Edited'Last - Insert'Length);
            Edited (Edit_At .. Edit_At + Insert'Length - 1) := Insert;
         end if;

         Edited_Last := Edited_Last - Delete'Length + Insert'Length;

         Edited_Buffer := To_Unbounded_String (Edited (1 .. Edited_Last));
      end Edit_Text;

      procedure To_KMN
        (Edit_At_Bytes : in Integer;
         Delete        : in String;
         Insert        : in String)
      is
         use WisiToken;

         Edit_At_Chars : constant Integer := Edit_At_Bytes - KMN_Next_Bytes + KMN_Next_Chars;

         Edit_1 : constant KMN :=
           (Stable_Bytes   => Base_Buffer_Pos (Edit_At_Bytes - KMN_Next_Bytes),
            Stable_Chars   => Base_Buffer_Pos (Edit_At_Chars - KMN_Next_Chars),
            Deleted_Bytes  => Delete'Length,
            Deleted_Chars  => Base_Buffer_Pos (UTF_8.Code_Point_Length (Delete)),
            Inserted_Bytes => Insert'Length,
            Inserted_Chars => Base_Buffer_Pos (UTF_8.Code_Point_Length (Insert)));
      begin
         Edits.Append (Edit_1);

         KMN_Next_Bytes := @ + Integer (Edit_1.Stable_Bytes + Edit_1.Deleted_Bytes);
         KMN_Next_Chars := @ + Integer (Edit_1.Stable_Chars + Edit_1.Deleted_Chars);
      end To_KMN;

      procedure Last_KMN
      is
         use WisiToken;
      begin
         if KMN_Next_Bytes <= Length (Initial_Buffer) then
            Edits.Append
              ((Stable_Bytes   => Base_Buffer_Pos (Length (Initial_Buffer) - KMN_Next_Bytes + 1),
                Stable_Chars   => Base_Buffer_Pos (Length (Initial_Buffer) - KMN_Next_Chars + 1),
                Deleted_Bytes  => 0,
                Deleted_Chars  => 0,
                Inserted_Bytes => 0,
                Inserted_Chars => 0));
         end if;
         --  EOI is also in a "stable region", but that is handled specially in
         --  Edit_Tree.
      end Last_KMN;

      procedure Put_Tree (Parser : in WisiToken.Parse.LR.Parser.Parser)
      is begin
         if Parser.Tree.Parents_Set then
            if Parser.Tree.Error_Count > 0 then
               New_Line;
               Parser.Put_Errors;
            end if;
         else
            if Parser.Tree.Error_Count (Parser.Tree.Last_Parse_Stream) > 0 then
               New_Line;
               Parser.Put_Errors (Parser.Tree.Last_Parse_Stream);
            end if;
         end if;

         New_Line;
         Put_Line (Label_Dot & " ... tree:");
         Parser.Tree.Print_Tree (Trace, Non_Grammar => True);
      end Put_Tree;

   begin
      --  Create Edited string
      if Initial /= "" then
         Initial_Buffer := To_Unbounded_String (Initial);
         Edited_Buffer  := To_Unbounded_String (Initial);
      else
         Initial_Buffer := Edited_Buffer; --  For To_KMN, Validate_KMN
      end if;

      --  Allow inserting after last char in Initial
      if Edit_At in 1 .. Length (Edited_Buffer) + 1 then
         if Edit_2_At in 1 .. Length (Edited_Buffer) + 1 then
            Edit_Text (Edit_2_At, Delete_2, Insert_2);
         end if;
         Edit_Text (Edit_At, Delete, Insert);
      end if;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line (Label_Dot & "initial source: '" & Initial & "'");
         Ada.Text_IO.Put_Line (Label_Dot & "edited source : '" & To_String (Edited_Buffer) & "'");
      end if;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Put_Line (Label_Dot & "edited source full parse:");
      end if;

      Full_Parser.Tree.Lexer.Reset_With_String (To_String (Edited_Buffer));

      begin
         Full_Parser.Parse (Log_File);
      exception
      when WisiToken.Syntax_Error =>
         if WisiToken.Trace_Tests > WisiToken.Outline then
            Put_Line (Label_Dot & "(syntax_error)");
            Put_Tree (Full_Parser);
         end if;
         Check ("syntax_error", True, False);
         return;
      end;

      Full_Parser.Tree.Copy_Tree (Edited_Source_Full_Parse_Tree, User_Data'Access);

      if WisiToken.Trace_Tests > WisiToken.Outline then
         Put_Tree (Full_Parser);
      end if;

      if Initial /= "" then
         if WisiToken.Trace_Tests > WisiToken.Outline then
            New_Line;
            Put_Line (Label_Dot & "initial source full parse:");
         end if;

         Incremental_Parser.Tree.Lexer.Reset_With_String (Initial);
         begin
            Incremental_Parser.Parse (Log_File);
         exception
         when WisiToken.Syntax_Error =>
            if WisiToken.Trace_Tests > WisiToken.Outline then
               Put_Line (Label_Dot & "(syntax_error)");
               Put_Tree (Incremental_Parser);
            end if;
            Check ("syntax_error", True, False);
            return;
         end;

         if WisiToken.Trace_Tests > WisiToken.Outline then
            Put_Tree (Incremental_Parser);
         end if;
         Check (Label_Dot & "full errors", Incremental_Parser.Tree.Error_Count, Full_Errors);
      end if;

      if WisiToken.Trace_Tests > WisiToken.Outline then
         New_Line;
         Put_Line (Label_Dot & "incremental parse:");
      end if;

      Incremental_Parser.Tree.Lexer.Reset_With_String (To_String (Edited_Buffer));

      if Edit_At in 1 .. Length (Edited_Buffer) then
         To_KMN (Edit_At, Delete, Insert);
         if Edit_2_At in 1 .. Length (Edited_Buffer) then
            To_KMN (Edit_2_At, Delete_2, Insert_2);
         end if;
         Last_KMN;
      end if;

      if Edits.Length > 0 then
         if WisiToken.Trace_Tests > WisiToken.Outline then
            Put_Line (Label_Dot & "KMN_List:" & Image (Edits));
         end if;

         Validate_KMN
           (List => Edits,
            Initial_Text_Byte_Region => (1, WisiToken.Base_Buffer_Pos (Length (Initial_Buffer))),
            Initial_Text_Char_Region =>
              (1, WisiToken.Base_Buffer_Pos (Length (Initial_Buffer))),
            --  FIXME: test utf-8
            Edited_Text_Byte_Region  => (1, WisiToken.Base_Buffer_Pos (Length (Edited_Buffer))),
            Edited_Text_Char_Region  => (1, WisiToken.Base_Buffer_Pos (Length (Edited_Buffer))));
      end if;

      Incremental_Parser.Parse (Log_File, Edits);

      if WisiToken.Trace_Tests > WisiToken.Outline then
         New_Line;
         Put_Line (Label_Dot & "incremental parse result:");
         Put_Tree (Incremental_Parser);
      end if;

      Check (Label_Dot & "incr errors", Incremental_Parser.Tree.Error_Count, Incr_Errors);
      Check (Label_Dot & "tree", Incremental_Parser.Tree, Edited_Source_Full_Parse_Tree,
             Shared_Stream         => False,
             Terminal_Node_Numbers => False);
   exception
   when AUnit.Assertions.Assertion_Error =>
      raise;

   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Tests > WisiToken.Outline then
         Put_Line (Label_Dot & "(syntax_error) incremental parse result:");
         Put_Tree (Incremental_Parser);
      end if;

      Check ("syntax_error", True, False);

   when E : WisiToken.Parse_Error =>
      AUnit.Assertions.Assert (False, "parse_error: " & Ada.Exceptions.Exception_Message (E));

   when E : others =>
      Ada.Text_IO.Put_Line
        ("unhandled exception: " & Ada.Exceptions.Exception_Name (E) & ": " &
           Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      AUnit.Assertions.Assert (False, "unhandled exception");
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
      --  Two edits in one token not at EOI
      Parse_Text
        (Initial   => "A := B + C; --  A very long comment" & ASCII.LF & "D;",
         --            1        |10       |20       |30       |36
         Edit_At   => 18,
         Delete    => "",
         Insert    => "nother",
         Edit_2_At => 24,
         Delete_2  => "long",
         Insert_2  => "big");

      --  edited: "A := B + C; --  Another very big comment" & ASCII.LF & "D;",
      --           1        |10       |20       |30       |40

   end Edit_Comment_2;

   procedure Edit_Comment_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Two edits in one token not EOI
      Parse_Text
        (Initial   => "A := B + C; --  A very long comment",
         --            1        |10       |20
         Edit_At   => 18,
         Delete    => "",
         Insert    => "nother",
         Edit_2_At => 24,
         Delete_2  => "long",
         Insert_2  => "big");
   end Edit_Comment_3;

   procedure Edit_Comment_4 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert end comment exposes code.
      --
      --  Preceding comment to ensure we don't mistake that for a new
      --  comment end.
      Parse_Text
        (Initial =>
           "-- preceding" & ASCII.LF &
           --  |4    |10    |13
           "-- A := B;" & ASCII.LF & "C;",
         --  |15  |20
         Edit_At => 17,
         Delete  => "",
         Insert  => "comment" & ASCII.LF);

      --  Edited text:
      --  -- preceding
      --  |1
      --  -- comment
      --  |14
      --  A := B;
      --  |25
      --  C;
      --  |33
   end Edit_Comment_4;

   procedure Edit_Comment_5 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Similar to Edit_Comment_4, tests a different case in Edit_Tree.
      Parse_Text
        (Initial =>
           "D;" & ASCII.LF &
           "-- preceding" & ASCII.LF &
           --  |7 |10       |16
           "-- A := B;" & ASCII.LF &
           --  |20  |25
           "C;",
         --  |29
         Edit_At => 20,
         Delete  => "",
         Insert  => "comment" & ASCII.LF);
   end Edit_Comment_5;

   procedure Edit_Comment_6 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-interactive_01.adb; capitalize a word in a comment.
      --  Found bug in comment_end_deleted logic.
      Parse_Text
        (Initial =>
           "procedure A is begin" & ASCII.LF &
           --        |10       |20
           "-- An_Identifier in a comment" & ASCII.LF &
           --  |25  |30       |40       |50
           "   A := B;" & ASCII.LF &
           "end A;",
         Edit_At => 25,
         Delete  => "A",
         Insert  => "a");
   end Edit_Comment_6;

   procedure Edit_Comment_7 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Similar to Edit_Comment_4, indent comment before editing it.
      Parse_Text
        (Initial   =>
           "D;" & ASCII.LF &
           "-- preceding" & ASCII.LF &
           --  |7 |10       |16
           "-- A := B;" & ASCII.LF & "C;",
         --  |18    |25
         Edit_At   => 17,
         Insert    => "   ",
         Delete    => "",
         Edit_2_At => 20,
         Delete_2  => "",
         Insert_2  => "comment" & ASCII.LF);

      --  Edited text:
      --  'D;
      --   |1
      --  -- preceding
      --  |4    |10
      --     -- comment
      --  |17     |25
      --  A := B;
      --  |31
      --  C;'
      --  |39
   end Edit_Comment_7;

   procedure Edit_Comment_8 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Edit comment by inserting before and after.
      Parse_Text
        (Initial   =>
           "D;" & ASCII.LF &
           "-- comment_1" & ASCII.LF &
           --  |7 |10       |16
           "   C;",
         --    |20
         Edit_At   => 4,
         Insert    => "   ",
         Delete    => "",
         Edit_2_At => 17,
         Delete_2  => "",
         Insert_2  => "   ");

      --  Edited text:
      --  'D;
      --   |1
      --     -- comment_1
      --  |4    |10
      --        -- comment_2
      --  |20       |30
      --  C;'
      --  |39
   end Edit_Comment_8;

   procedure Edit_Comment_9 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Edit comment not adjacent to next token should not delete next
      --  token. Reproduces test case in ada_mode-interactive_07.adb.
      Parse_Text
        (Initial   =>
           "package A is procedure D;" & ASCII.LF &
             --      |10       |20       |26
             "   -- Return the terminal" & ASCII.LF &
             --  |30       |40       |50   |52
             "   -- 1. foo bar" & ASCII.LF &
             --  |56              |66
             "   -- Result is" & ASCII.LF &
             --            |80   |82
             "   -- by Tree." & ASCII.LF &
             "   procedure C; end A;",
         Edit_At   => 62,
         Insert    => "",
         Delete    => "foo bar" & ASCII.LF);

   end Edit_Comment_9;

   procedure Edit_Whitespace_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
           --     |40       |50       |60
           "end Test;",
           --   |70
         Edit_At   => 19,
         Delete    => " ",
         Insert    => "",
         Edit_2_At => 36,
         Delete_2  => " ",
         Insert_2  => "");

   end Edit_Whitespace_1;

   procedure Edit_Whitespace_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Two edits in one whitespace
      Parse_Text
        (Initial   =>
           "package Test is" & ASCII.LF &
           --        |10       |16
           "   --  A comment" & ASCII.LF &
           --  |20       |30
           "     function Bar return Integer;" & ASCII.LF &
           --     |40       |50       |60
           "end Test;",
           --   |70
         Edit_At   => 34,
         Delete    => " ",
         Insert    => "",
         Edit_2_At => 36,
         Delete_2  => " ",
         Insert_2  => "");

   end Edit_Whitespace_2;

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
      --  Edit point affects two adjacent tokens
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

   procedure Edit_Code_9 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Final stable covers several tokens. modeled on ada_mode-ada2012.ads
      Parse_Text
        (Initial => "-- comment" & ASCII.LF & "exit;" & ASCII.LF & "exit;",
         --          |1       |10              |12
         Edit_At => 12,
         Delete  => "exit",
         Insert  => "EXIT");
   end Edit_Code_9;

   procedure Delete_New_Line (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete a new_line
      Parse_Text
        (Initial => "A := B + C;" & ASCII.LF & "D;",
         --          |1       |10
         Edit_At => 12,
         Delete  => "" & ASCII.LF,
         Insert  => "");
   end Delete_New_Line;

   procedure Delete_Comment_End (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete a new_line that ends a comment, converting code into comment.
      Parse_Text
        (Initial => "A := B + C; -- comment" & ASCII.LF &
           --        |1       |10       |20
           "D (2);" & ASCII.LF &
           --         |30
           "C;",
         Edit_At => 23,
         Delete  => "" & ASCII.LF,
         Insert  => "");

      --  Edited: "A := B + C; -- commentD (2);" & ASCII.LF & "C;"
      --           |1       |10       |20          |29
   end Delete_Comment_End;

   procedure Delete_Comment_Start (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete a comment start, converting into code.
      Parse_Text
        (Initial => "A := B + C; " & ASCII.LF &
           --        |1       |10    |13
           "-- D;" & ASCII.LF &
           --  |17
           "C;",
         --  |21
         Edit_At => 14,
         Delete  => "--",
         Insert  => "");
   end Delete_Comment_Start;

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
         Tree : WisiToken.Syntax_Trees.Tree renames Incremental_Parser.Tree;
         Begin_Name_Node : constant Valid_Node_Access := Tree.Find_Descendant (Tree.Root, +subprogram_specification_ID);
         End_Name_Node   : constant Valid_Node_Access := Tree.Find_Descendant (Tree.Root, +name_opt_ID);
      begin
         Check
           ("name",
            Tree.Lexer.Buffer_Text (Tree.Name (Begin_Name_Node)),
            Tree.Lexer.Buffer_Text (Tree.Name (End_Name_Node)));
      end;
   end Names;

   procedure Missing_Name_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test that error due to Missing_Name is dropped when incremental
      --  edit fixes it.
      Ada_Lite_Actions.End_Name_Optional := False;

      Parse_Text
        (Initial => "procedure Name is begin null; end;",
         --          |1       |10       |20       |30

         Edit_At           => 34,
         Delete            => "",
         Insert            => " Name",
         Full_Errors => 1,
         Incr_Errors => 0);

   end Missing_Name_1;

   procedure Recover_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Full parse uses error recovery to place missing return type.
      --  Incremental parse fixes the error.
      Parse_Text
        (Initial           =>
           "function Func_1 (A : Integer) return " & ASCII.LF &
           --        |10       |20       |30
           "is begin return 1; end;",
         Edit_At           => 38,
         Delete            => "",
         Insert            => "Integer",
         Full_Errors => 1,
         Incr_Errors => 0);
   end Recover_1;

   procedure Recover_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Full parse encounters an error on "." in "end Pkg.Proc_1"; error
      --  recovery finishes the block started by "begin". Incremental parse
      --  edit fixes the error; main parser must delete the error on "."
      --  when the name_opt nonterm is shifted.
      Ada_Lite_Actions.End_Name_Optional := False;

      Parse_Text
        (Initial           =>
           "procedure Pkg.Proc_1 is begin A; begin end Pkg.Proc_1;",
           --        |10       |20       |30       |40       |50
         Edit_At           => 40,
         Delete            => "",
         Insert            => "end; ",
         Full_Errors => 1,
         Incr_Errors => 0);
   end Recover_2;

   procedure Lexer_Errors_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Missing string quote. Initial full parse recovers from it,
      --  incremental edit does not fix it.
      Parse_Text
        (Initial          =>
           "A := 2;" & ASCII.LF &
             --  |6
             "B := ""A string" & ASCII.LF & -- missing '";'; lexer error at 14
             --  |12      |20
             "C := 1;",
         --   |23
         Edit_At      => 6,
         Delete       => "",
         Insert       => "33",
         Full_Errors => 2,  --  Lexer + parser.
         Incr_Errors => 2); --  Errors are still in tree
   end Lexer_Errors_1;

   procedure Preserve_Parse_Errors_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Full parse reports a parse error (missing ';'). Incremental parse
      --  does not fix it, so the error is still in the tree.
      Parse_Text
        (Initial => "A := 2" & ASCII.LF,
         --          |1   |6

         Edit_At      => 2,
         Delete       => "",
         Insert       => "3",
         Full_Errors => 1,
         Incr_Errors => 1);
   end Preserve_Parse_Errors_1;

   procedure Preserve_Parse_Errors_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Full parse reports a parse error (extra 'end;'). Incremental parse
      --  fixes the error by editing in a different location. This
      --  demonstrates the need for storing parse errors as nodes in the
      --  syntax tree, and for always deleting error corrections even in
      --  non-edit regions.
      Parse_Text
        (Initial => "A := 2; end;" & ASCII.LF,
         --          |1      |9

         Edit_At           => 1,
         Delete            => "",
         Insert            => "begin ",
         Full_Errors => 1,
         Incr_Errors => 0);
   end Preserve_Parse_Errors_2;

   procedure Modify_Deleted_Node (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite_Actions;
      use AUnit.Checks.Containers;
      use WisiToken.AUnit;
   begin
      --  Derived from ada_mode-interactive_02.adb insert new comment
      --  slowly. In the first parse, the node for the inserted "-" is
      --  deleted by error recovery. In the second parse, the node is
      --  restored and then scanned and replaced by the comment.
      Parse_Text
        (Label        => "1",
         Initial      =>
           "A := 2;" & ASCII.LF &
           --  |6
           ASCII.LF,
         --  | 9

         Edit_At      => 9,
         Delete       => "",
         Insert       => "-", -- start a comment
         Full_Errors => 0,
         Incr_Errors => 1);

      declare
         use WisiToken;
         use WisiToken.Syntax_Trees;
         use AUnit.Checks;
         Tree : Syntax_Trees.Tree renames Incremental_Parser.Tree;
         Deleted : Node_Access := Tree.First_Terminal (Tree.Root);
      begin
         loop
            exit when Deleted = Invalid_Node_Access;
            exit when Tree.Label (Deleted) = Source_Terminal and then Tree.Has_Following_Deleted (Deleted);
            Deleted := Tree.Next_Terminal (@);
         end loop;

         Check ("no deleted found", Deleted = Invalid_Node_Access, False);

         declare
            Deleted_Nodes : Valid_Node_Access_Lists.List renames Tree.Following_Deleted (Deleted);
         begin
            Check ("deleted count", Deleted_Nodes.Length, 1);
            Check ("deleted '-'", Tree.ID (Deleted_Nodes (Deleted_Nodes.First)), +MINUS_ID);
         end;

         loop
            Deleted := Tree.Next_Terminal (@);
            exit when Deleted = Invalid_Node_Access;
            exit when Tree.Label (Deleted) = Source_Terminal and then Tree.Has_Following_Deleted (Deleted);
         end loop;

         Check ("more deleted found", Deleted = Invalid_Node_Access, True);
      end;

      Parse_Text
        (Label             => "2",
         Initial           => "",            -- continue from previous
         Edit_At           => 10,
         Delete            => "",
         Insert            => "- a comment", -- finish comment
         Full_Errors => 1,
         Incr_Errors => 0);

   end Modify_Deleted_Node;

   procedure Multiple_Errors_On_One_Token_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Same initial text as test_mckenzie_recover.adb
      --  Multiple_Errors_On_One_Token_1; here we edit to fix the error.

      Parse_Text
        ("procedure A is B : Integer" & ASCII.LF &
           --  |6  |10       |20
           "procedure C is begin null; end A; procedure D is begin null; end D;",
         --  |29        |40       |50       |60       |70       |80       |90
         Edit_At     => 27,
         Delete      => "",
         Insert      => ";",
         Edit_2_At   => 54,
         Delete_2    => "",
         Insert_2    => " end C; begin null;",
         Full_Errors => 2,
         Incr_Errors => 0);

      --  Edited text:
      --  "procedure A is B : Integer;" & ASCII.LF &
      --   |1       |10       |20
      --  "procedure C is begin null; end C; begin null; end A; procedure D is begin null; end D;",
      --   |29        |40       |50       |60       |70       |80       |90       |100      |110

      --  There are two errors; missing ';' after 'Integer',
      --  missing_name_error on 'procedure C'. The solution to the
      --  match_names_error requires unreducing subprogram_body C, so both
      --  errors store an error on 'procedure', which are cleared during
      --  incremental parse after the edits.
   end Multiple_Errors_On_One_Token_1;

   procedure Multiple_Errors_On_One_Token_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Same initial text as test_mckenzie_recover.adb
      --  Multiple_Errors_On_One_Token_1; here we have a different edit
      --  that breaks down procedure C, so 'procedure' is in the input
      --  stream as a terminal.

      Parse_Text
        ("procedure A is B : Integer" & ASCII.LF &
           --  |6  |10       |20
           "procedure C is begin E; end A; procedure D is begin E; end D;",
         --  |29        |40       |50       |60       |70       |80    |90
         Edit_At     => 27,
         Delete      => "",
         Insert      => ";",
         Edit_2_At   => 38,
         Delete_2    => "C",
         Insert_2    => "C1",
         Full_Errors => 2,
         Incr_Errors => 1);

      --  Edited text:
      --  "procedure A is B : Integer;" & ASCII.LF &
      --   |1       |10       |20
      --  "procedure C1 is begin E; end A; procedure D is begin E; end D;",
      --   |29        |40       |50       |60       |70       |80       |90
   end Multiple_Errors_On_One_Token_2;

   procedure Non_Ascii (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert, delete non-ASCII text.

      Parse_Text
        ("package A is Theta : Wide_Character := ""Θ""; B : F := ""π_Non""; end A;",
         --    |6  |10       |20       |30       |40
         Edit_At     => 41,
         Delete      => "",
         Insert      => "Π",
         Edit_2_At   => 56,
         Delete_2    => "π_",
         Insert_2    => "pi_",
         Full_Errors => 0,
         Incr_Errors => 0);
   end Non_Ascii;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Change'Access, "No_Change");
      Register_Routine (T, Edit_Comment'Access, "Edit_Comment");
      Register_Routine (T, Edit_Comment_2'Access, "Edit_Comment_2");
      Register_Routine (T, Edit_Comment_3'Access, "Edit_Comment_3");
      Register_Routine (T, Edit_Comment_4'Access, "Edit_Comment_4");
      Register_Routine (T, Edit_Comment_5'Access, "Edit_Comment_5");
      Register_Routine (T, Edit_Comment_6'Access, "Edit_Comment_6");
      Register_Routine (T, Edit_Comment_7'Access, "Edit_Comment_7");
      Register_Routine (T, Edit_Comment_8'Access, "Edit_Comment_8");
      Register_Routine (T, Edit_Comment_9'Access, "Edit_Comment_9");
      Register_Routine (T, Edit_Whitespace_1'Access, "Edit_Whitespace_1");
      Register_Routine (T, Edit_Whitespace_2'Access, "Edit_Whitespace_2");
      Register_Routine (T, Edit_Leading_Non_Grammar'Access, "Edit_Leading_Non_Grammar");
      Register_Routine (T, Edit_Code_1'Access, "Edit_Code_1");
      Register_Routine (T, Edit_Code_2'Access, "Edit_Code_2");
      Register_Routine (T, Edit_Code_3'Access, "Edit_Code_3");
      Register_Routine (T, Edit_Code_4'Access, "Edit_Code_4");
      Register_Routine (T, Edit_Code_5'Access, "Edit_Code_5");
      Register_Routine (T, Edit_Code_6'Access, "Edit_Code_6");
      Register_Routine (T, Edit_Code_7'Access, "Edit_Code_7");
      Register_Routine (T, Edit_Code_8'Access, "Edit_Code_8");
      Register_Routine (T, Edit_Code_9'Access, "Edit_Code_9");
      Register_Routine (T, Delete_New_Line'Access, "Delete_New_Line");
      Register_Routine (T, Delete_Comment_End'Access, "Delete_Comment_End");
      Register_Routine (T, Delete_Comment_Start'Access, "Delete_Comment_Start");
      Register_Routine (T, Insert_New_Line'Access, "Insert_New_Line");
      Register_Routine (T, Names'Access, "Names");
      Register_Routine (T, Missing_Name_1'Access, "Missing_Name_1");
      Register_Routine (T, Recover_1'Access, "Recover_1");
      Register_Routine (T, Recover_2'Access, "Recover_2");
      Register_Routine (T, Lexer_Errors_1'Access, "Lexer_Errors_1");
      Register_Routine (T, Preserve_Parse_Errors_1'Access, "Preserve_Parse_Errors_1");
      Register_Routine (T, Preserve_Parse_Errors_2'Access, "Preserve_Parse_Errors_2");
      Register_Routine (T, Modify_Deleted_Node'Access, "Modify_Deleted_Node");
      Register_Routine (T, Multiple_Errors_On_One_Token_1'Access, "Multiple_Errors_On_One_Token_1");
      Register_Routine (T, Multiple_Errors_On_One_Token_2'Access, "Multiple_Errors_On_One_Token_2");
      Register_Routine (T, Non_Ascii'Access, "Non_Ascii");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_incremental.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      --  Run before all tests in register
      WisiToken.Parse.LR.Parser.New_Parser
        (Full_Parser,
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
        (Incremental_Parser,
         Trace'Access,
         Ada_Lite_LR1_T1_Main.Create_Lexer,
         Ada_Lite_LR1_T1_Main.Create_Parse_Table
           (Text_Rep_File_Name          => "ada_lite_lr1_t1_re2c_parse_table.txt"),
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      if T.McKenzie_Config /= null then
         WisiToken.Parse.LR.Set_McKenzie_Options (Incremental_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
         WisiToken.Parse.LR.Set_McKenzie_Options (Full_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
      end if;
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      Ada_Lite_Actions.End_Name_Optional := True;
   end Set_Up;

end Test_Incremental;
--  Local Variables:
--  ada-case-strict: nil
--  End:
