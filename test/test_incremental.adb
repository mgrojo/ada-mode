--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2022 Stephen Leake.  All Rights Reserved.
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

with Grammar_Grammar_01_Actions;
with Grammar_Grammar_01_LR1_T1_Main;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite_Ebnf;
with WisiToken.Parse.LR.McKenzie_Recover.Grammar_Grammar_01;
with AUnit.Assertions;
with AUnit.Checks.Containers;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_Ebnf_Actions;
with Ada_Lite_Ebnf_LALR_Main;
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

   package Ada_Lite is
      Incremental_Parser : aliased WisiToken.Parse.LR.Parser.Parser;
      Full_Parser        : aliased WisiToken.Parse.LR.Parser.Parser;

      Orig_McKenzie_Param : WisiToken.Parse.LR.McKenzie_Param_Type
        (Ada_Lite_Actions.Descriptor.First_Terminal,
         Ada_Lite_Actions.Descriptor.Last_Terminal,
         Ada_Lite_Actions.Descriptor.First_Nonterminal,
         Ada_Lite_Actions.Descriptor.Last_Nonterminal);
   end Ada_Lite;

   package Ada_Lite_EBNF is
      Incremental_Parser : aliased WisiToken.Parse.LR.Parser.Parser;
      Full_Parser        : aliased WisiToken.Parse.LR.Parser.Parser;

      Orig_McKenzie_Param : WisiToken.Parse.LR.McKenzie_Param_Type
        (Ada_Lite_Ebnf_Actions.Descriptor.First_Terminal,
         Ada_Lite_Ebnf_Actions.Descriptor.Last_Terminal,
         Ada_Lite_Ebnf_Actions.Descriptor.First_Nonterminal,
         Ada_Lite_Ebnf_Actions.Descriptor.Last_Nonterminal);
   end Ada_Lite_EBNF;

   package Grammar is
      Incremental_Parser : aliased WisiToken.Parse.LR.Parser.Parser;
      Full_Parser        : aliased WisiToken.Parse.LR.Parser.Parser;

      Orig_McKenzie_Param : WisiToken.Parse.LR.McKenzie_Param_Type
        (Grammar_Grammar_01_Actions.Descriptor.First_Terminal,
         Grammar_Grammar_01_Actions.Descriptor.Last_Terminal,
         Grammar_Grammar_01_Actions.Descriptor.First_Nonterminal,
         Grammar_Grammar_01_Actions.Descriptor.Last_Nonterminal);
   end Grammar;

   Initial_Buffer : Ada.Strings.Unbounded.Unbounded_String;
   Edited_Buffer  : Ada.Strings.Unbounded.Unbounded_String;

   Incremental_Parser : access WisiToken.Parse.LR.Parser.Parser := Ada_Lite.Incremental_Parser'Access;
   Full_Parser        : access WisiToken.Parse.LR.Parser.Parser := Ada_Lite.Full_Parser'Access;

   procedure Parse_Text
     (Initial        : in String;
      Edit_At        : in Integer;
      Delete         : in String;
      Insert         : in String;
      Edit_2_At      : in Integer                   := 0;
      Delete_2       : in String                    := "";
      Insert_2       : in String                    := "";
      Initial_Errors : in Ada.Containers.Count_Type := 0;
      Incr_Errors    : in Ada.Containers.Count_Type := 0;
      Optimized_List : in Boolean                   := False;
      Label          : in String                    := "")
   with Pre => Edit_2_At = 0 or Edit_2_At >= Edit_At
   --  If Initial is "", start from previous edited text and existing tree.
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
         Put_Line (Label_Dot & "tree:");
         Parser.Tree.Print_Tree (Non_Grammar => True);
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

      if WisiToken.Trace_Tests > WisiToken.Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line (Label_Dot & "initial source: '" & Initial & "'");
         Ada.Text_IO.Put_Line (Label_Dot & "edited source : '" & To_String (Edited_Buffer) & "'");
      end if;

      if WisiToken.Trace_Tests > WisiToken.Detail or
        WisiToken.Trace_McKenzie + WisiToken.Trace_Parse > WisiToken.Detail
      then
         Put_Line (Label_Dot & "edited source full parse:");
      end if;

      Full_Parser.Tree.Lexer.Reset_With_String (To_String (Edited_Buffer));
      Full_Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);

      begin
         Full_Parser.Parse (Log_File);
         if WisiToken.Trace_Tests > WisiToken.Detail then
            Put_Tree (Full_Parser.all);
         end if;
         Check (Label_Dot & "edited full parse errors", Full_Parser.Tree.Error_Count, Incr_Errors);
      exception
      when WisiToken.Syntax_Error =>
         Check ("syntax_error", True, False);
         return;
      end;

      Full_Parser.Tree.Copy_Tree (Edited_Source_Full_Parse_Tree, User_Data'Access);

      if Initial /= "" then
         if WisiToken.Trace_Tests > WisiToken.Detail or
           WisiToken.Trace_McKenzie + WisiToken.Trace_Parse > WisiToken.Detail
         then
            New_Line;
            Put_Line (Label_Dot & "initial source full parse:");
         end if;

         Incremental_Parser.Tree.Lexer.Reset_With_String (Initial);
         Incremental_Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);
         begin
            Incremental_Parser.Parse (Log_File);
            if WisiToken.Trace_Tests > WisiToken.Detail then
               Put_Tree (Incremental_Parser.all);
            end if;
         exception
         when WisiToken.Syntax_Error =>
            Check ("syntax_error", True, False);
            return;
         end;

         Check (Label_Dot & "initial errors", Incremental_Parser.Tree.Error_Count, Initial_Errors);
      end if;

      if WisiToken.Trace_Tests > WisiToken.Detail or
        WisiToken.Trace_McKenzie + WisiToken.Trace_Parse > WisiToken.Detail
      then
         New_Line;
         Put_Line (Label_Dot & "incremental parse:");
      end if;

      Incremental_Parser.Tree.Lexer.Reset_With_String (To_String (Edited_Buffer));
      Incremental_Parser.Tree.Lexer.Set_Verbosity (WisiToken.Trace_Lexer - 1);

      if Edit_At in 1 .. Length (Edited_Buffer) then
         To_KMN (Edit_At, Delete, Insert);
         if Edit_2_At in 1 .. Length (Edited_Buffer) then
            To_KMN (Edit_2_At, Delete_2, Insert_2);
         end if;
         Last_KMN;
      end if;

      if Edits.Length > 0 then
         if WisiToken.Trace_Tests > WisiToken.Detail then
            Put_Line (Label_Dot & "KMN_List:" & Image (Edits));
         end if;

         Validate_KMN
           (List => Edits,
            Initial_Text_Byte_Region => (1, WisiToken.Base_Buffer_Pos (Length (Initial_Buffer))),
            Initial_Text_Char_Region =>
              (1, WisiToken.Base_Buffer_Pos (Length (Initial_Buffer))),
            Edited_Text_Byte_Region  => (1, WisiToken.Base_Buffer_Pos (Length (Edited_Buffer))),
            Edited_Text_Char_Region  => (1, WisiToken.Base_Buffer_Pos (Length (Edited_Buffer))));
      end if;

      Incremental_Parser.Parse (Log_File, Edits);

      if WisiToken.Trace_Tests > WisiToken.Detail then
         New_Line;
         Put_Line (Label_Dot & "incremental parse result:");
         Put_Tree (Incremental_Parser.all);
      end if;

      Check (Label_Dot & "incr errors", Incremental_Parser.Tree.Error_Count, Incr_Errors);

      declare
         Error_Reported : WisiToken.Syntax_Trees.Node_Sets.Set;
      begin
         Incremental_Parser.Tree.Validate_Tree
           (User_Data, Error_Reported,
            Node_Index_Order => False,
            Validate_Node    => WisiToken.Syntax_Trees.Mark_In_Tree'Access);
         Incremental_Parser.Tree.Free_Augmented;
         if Error_Reported.Count > 0 then
            AUnit.Assertions.Assert (False, "incr invalid tree");
         end if;
      end;

      if not Optimized_List then
         --  Incremental parse can leave an optimized_list node in the tree;
         --  full parse does not. So we can't compare them
         Check (Label_Dot & "tree", Incremental_Parser.Tree, Edited_Source_Full_Parse_Tree,
                Shared_Stream         => False,
                Terminal_Node_Numbers => False);
      end if;

   exception
   when AUnit.Assertions.Assertion_Error =>
      raise;

   when WisiToken.Syntax_Error =>
      if WisiToken.Trace_Tests > WisiToken.Detail then
         Put_Line (Label_Dot & "(syntax_error) incremental parse result:");
         Put_Tree (Incremental_Parser.all);
      end if;

      Check ("syntax_error", True, False);

   when E : WisiToken.Parse_Error =>
      if WisiToken.Debug_Mode then
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      end if;
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

   procedure Edit_Comment_02 (T : in out AUnit.Test_Cases.Test_Case'Class)
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

   end Edit_Comment_02;

   procedure Edit_Comment_03 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Comment_03;

   procedure Edit_Comment_04 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert end comment exposes code; extend existing comment confuses
      --  New_Code_End compute.
      --
      --  Preceding comment to ensure we don't mistake that for a new
      --  comment end.
      Parse_Text
        (Initial =>
           "-- preceding" & ASCII.LF &
             --  |6  |10    |13
             "-- A := B;" & ASCII.LF & "C;",
         --    |15  |20     |24
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
   end Edit_Comment_04;

   procedure Edit_Comment_05 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Similar to Edit_Comment_04, tests a different case in Edit_Tree.
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
   end Edit_Comment_05;

   procedure Edit_Comment_06 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Comment_06;

   procedure Edit_Comment_07 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Similar to Edit_Comment_04, indent comment before editing it.
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
   end Edit_Comment_07;

   procedure Edit_Comment_08 (T : in out AUnit.Test_Cases.Test_Case'Class)
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

   end Edit_Comment_08;

   procedure Edit_Comment_09 (T : in out AUnit.Test_Cases.Test_Case'Class)
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

   end Edit_Comment_09;

   procedure Edit_Comment_10 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-interactive_02
      Parse_Text
        (Initial   =>
           "package A is" & ASCII.LF &
             --      |10     |13
             "   -- comment 1" & ASCII.LF &
             --     |20          |29
             "   -- comment 2" & ASCII.LF &
             --    |35           |45
             "" & ASCII.LF &
             --   |46
             "   " & ASCII.LF &
             --      |50
             "   -- comment 3" & ASCII.LF &
             --   |55       |65
             "   procedure C; end A;",
         --   |67
         Edit_At   => 47,
         Insert    => "   ",
         Delete    => "",
         Edit_2_At   => 67,
         Insert_2    => "" & ASCII.LF,
         Delete_2    => "   ");
   end Edit_Comment_10;

   procedure Edit_Comment_11 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Another case from ada_mode-interactive_02
      Parse_Text
        (Initial   =>
           "package A is" & ASCII.LF &
             --      |10     |13
             "   -- comment 1" & ASCII.LF &
             --     |20          |29
             "   --" & ASCII.LF &
             --        |35
             "   -- comment 3" & ASCII.LF &
             --   |40       |50
             "   procedure C; end A;",

         Edit_At   => 33,
         Insert    => "",
         Delete    => "--" & ASCII.LF,
         Edit_2_At => 65,
         Insert_2  => "c",
         Delete_2  => "C");
   end Edit_Comment_11;

   procedure Edit_Comment_12 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-interactive_13.adb; insert comment end in SOI
      --  comment, before other comment.
      Parse_Text
        (Initial   =>
           "-- Commentpackage A is" & ASCII.LF &
             --      |10
             "   -- comment 2" & ASCII.LF &
             "end A;",

         Edit_At        => 11,
         Insert         => "" & ASCII.LF,
         Delete         => "",
         Initial_Errors => 1,
         Incr_Errors    => 0);
   end Edit_Comment_12;

   procedure Edit_Comment_13 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-interactive_09.adb. Scanned comments cross two KMN,
      --  confusing shift computations.
      Parse_Text
        (Initial   =>
           "procedure A" & ASCII.LF &
             --      |10
             "is" & ASCII.LF &
             --     |15
             "   -- comment 1" & ASCII.LF &
             --  |19
             "   -- comment 2" & ASCII.LF &
             --  |35
             "begin null; end A;",

         Edit_At        => 19,
         Insert         => "   ",
         Delete         => "",
         Edit_2_At      => 35,
         Insert_2       => "   ",
         Delete_2       => "",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Edit_Comment_13;

   procedure Edit_Comment_14 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode.adb.
      Parse_Text
        (Initial   =>
           "procedure Ask" & ASCII.LF &
             --      |10
             "is begin null;" & ASCII.LF &
             --    |20          |29
             "   -- comment 1" & ASCII.LF &
             --  |33    |40      |45
             "   -- comment 2" & ASCII.LF &
             --  |49        |60
             "end Ask;",
         --       |66

         Edit_At        => 33,
         Insert         => "   ",
         Delete         => "",
         Edit_2_At      => 66,
         Insert_2       => "A",
         Delete_2       => "A",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Edit_Comment_14;

   procedure Edit_Comment_15 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Edit modifies comment start
      Parse_Text
        (Initial   =>
           "procedure Ask" & ASCII.LF &
             --      |10
             "is begin" & ASCII.LF &
             --    |20    |23
             "   A :=" & ASCII.LF &
             --     |30
             "   --" & ASCII.LF &
             --  |35
             "   bar;" & ASCII.LF &
             "end Ask;",
         --       |66

         Edit_At        => 36,
         Insert         => " foo ",
         Delete         => "",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Edit_Comment_15;

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

   procedure Edit_Whitespace_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-nested_packages.adb; insert whitespace after a
      --  token that is shorter than the inserted text.
      Parse_Text
        (Initial   => "procedure Test is begin null; end Test;",
         --            |1       |10       |20       |30
         Edit_At   => 30,
         Delete    => "",
         Insert    => "   ");
   end Edit_Whitespace_3;

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

   procedure Edit_Code_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_01;

   procedure Edit_Code_02 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_02;

   procedure Edit_Code_03 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_03;

   procedure Edit_Code_04 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_04;

   procedure Edit_Code_05 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_05;

   procedure Edit_Code_06 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_06;

   procedure Edit_Code_07 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete whitespace after previous edit, possibly modifying '+' token
      Parse_Text
        (Label   => "1",
         Initial => "A := B +  C;",
         --          |1       |10
         Edit_At => 2,
         Delete  => "",
         Insert  => "_1",
         --          |2

         Edit_2_At => 9,
         Delete_2  => " ",
         Insert_2  => "");

      --  Same, but not modifying any token
      Parse_Text
        (Label   => "2",
         Initial => "A := B +   C;",
         --          |1       |10
         Edit_At => 2,
         Delete  => "",
         Insert  => "_1",
         --          |2

         Edit_2_At => 10,
         Delete_2  => " ",
         Insert_2  => "");
   end Edit_Code_07;

   procedure Edit_Code_08 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_08;

   procedure Edit_Code_09 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Edit_Code_09;

   procedure Edit_Code_10 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Simplified from ada_mode-interactive_10.adb
      Parse_Text
        (Initial        => "procedure Foo is a : integer" & ASCII.LF & ASCII.LF & "begin exit; end Foo;",
         --                 |1       |10       |20          |29
         Edit_At        => 22,
         Delete         => "i",
         Insert         => "I",
         Edit_2_At      => 29,
         Delete_2       => "",
         Insert_2       => ";",
         Initial_Errors => 1);
   end Edit_Code_10;

   procedure Edit_Code_11 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Similar to Edit_Code_10, tests another case in Edit_Tree.
      Parse_Text
        (Initial        => "procedure Foo is a : integer" & ASCII.LF & "begin" & ASCII.LF & "exit; end Foo;",
         --                 |1       |10       |20          |29
         Edit_At        => 31,
         Delete         => "e",
         Insert         => "e",
         Initial_Errors => 1,
         Incr_Errors    => 1);
   end Edit_Code_11;

   procedure Edit_Code_12 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  from ada_mode-interactive_02.adb
      Parse_Text
        (Initial        =>
           "if (A and B" & ASCII.LF &
             --      |10   |12
             "  -- Comment 1" & ASCII.LF &
             --      |20        |27
             ")" & ASCII.LF &
             "  -- comment 2" & ASCII.LF &
             "  or C then null; end if;",
         Edit_At        => 11,
         Delete         => "B",
         Insert         => "B",
         Edit_2_At      => 13,
         Delete_2       => "  -- Comment 1" & ASCII.LF &  ")",
         Initial_Errors => 0,
         Incr_Errors    => 1);
   end Edit_Code_12;

   procedure Edit_Code_13 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Just indent
      Parse_Text
        (Initial        => "   null;",
         Edit_At        => 1,
         Delete         => "   ",
         Insert         => "   ");
   end Edit_Code_13;

   procedure Edit_Code_14 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-nominal-child.adb
      Parse_Text
        (Initial        => "package body A is function B return Float;end A;",
         --                 |1       |10       |20       |30       |40
         Edit_At        => 42,
         Delete         => "",
         Insert         => " is begin end B",
         Edit_2_At      => 43,
         Delete_2       => "",
         Insert_2       => "" & ASCII.LF,
         Initial_Errors => 0,
         Incr_Errors    => 1);
   end Edit_Code_14;

   procedure Edit_Code_15 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Just indent
      Parse_Text
        (Initial        => "procedure A is begin" & ASCII.LF & "return B; null; end A;",
         --                 |1       |10       |20
         Edit_At        => 22,
         Delete         => "",
         Insert         => "   ");
   end Edit_Code_15;

   procedure Edit_Code_16 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  From ada_mode-interactive_11.adb before we turned off recase test;
      --  token extends beyond first change region, second change region
      --  inserts following non_grammar.
      Parse_Text
        (Initial   => "package C is function A return B_Type; end C;",
         --            |1       |10       |20       |30       |40
         Edit_At   => 32,
         Delete    => "B_T",
         Insert    => "b_t",
         Edit_2_At => 38,
         Delete_2    => "",
         Insert_2    => "" & ASCII.LF);
   end Edit_Code_16;

   procedure Edit_Code_17 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Another case from ada_mode-interactive_11.adb; refining boundary
      --  conditions in Edit_Tree.
      Parse_Text
        (Initial   => "package C is function A (B : int)return" & ASCII.LF &
           --          |1       |10       |20       |30           |40
           "  B_Type; end C;",
         Edit_At   => 34,
         Delete    => "r",
         Insert    => "r",
         Edit_2_At => 40,
         Delete_2  => ASCII.LF & "  B_Type",
         Insert_2  => ASCII.LF & "  B_Type");
   end Edit_Code_17;

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
        (Label   => "1",
         Initial => "A := B + C; -- comment" & ASCII.LF &
           --        |1       |10       |20
           "D (2);" & ASCII.LF &
           --         |30
           "C;",
         Edit_At => 23,
         Delete  => "" & ASCII.LF,
         Insert  => "");
      --  Edited: "A := B + C; -- commentD (2);" & ASCII.LF & "C;"
      --           |1       |10       |20          |29

      --  This one does not delete a comment end.
      Parse_Text
        (Label   => "2",
         Initial => "A := B + C; -- comment" & ASCII.LF &
           --        |1       |10       |20
           "D (2);" & ASCII.LF &
           --         |30
           "C;",
         Edit_At => 20,
         Delete  => "ent",
         Insert  => "");
      --  Edited: "A := B + C; -- comm" & ASCII.LF & "D (2);" & ASCII.LF & "C;"
      --           |1       |10           |20                   |27
   end Delete_Comment_End;

   procedure Delete_Comment_Start_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
   end Delete_Comment_Start_01;

   procedure Delete_Comment_Start_02 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  This one does _not_ delete a comment start. From ada_mode-interactive_01.adb.
      Parse_Text
        (Initial   => "A := B + C; " & ASCII.LF &
           --        |1       |10    |13
           "-- ada_identifier" & ASCII.LF &
           --  |17          |30
           "C;",
         Edit_At   => 17,
         Delete    => "a",
         Insert    => "A",
         Edit_2_At => 21,
         Delete_2  => "i",
         Insert_2  => "I");
   end Delete_Comment_Start_02;

   procedure Delete_Comment_Start_03 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  This one modifies a comment start, which is the same as deleting
      --  it. Full parse of edited source finds different error recover
      --  solution from incremental parse unless we tweak things.

      Parse_Text
        (Label   => "3",
         Initial => "A := B + C; " & ASCII.LF &
           --        |1       |10    |13
           "-- ada_identifier;" & ASCII.LF &
           --  |17          |30
           "C;",

         Edit_At        => 15,
         Delete         => "-",
         Insert         => "",
         Initial_Errors => 0,
         Incr_Errors    => 1); -- "- ada_identifier" is not a statement.
   end Delete_Comment_Start_03;

   procedure Delete_Comment_Start_04 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  This one deletes part of a token, all of a following comment, and
      --  the comment start of the second comment.
      Parse_Text
        (Label   => "3",
         Initial => "A := B_1 " & ASCII.LF &
           --        |1           |10
           "-- comment" & ASCII.LF &
           "-- + ada_identifier" & ASCII.LF &
           " + C;",

         Edit_At        => 7,
         Delete         => "_1 " & ASCII.LF & "-- comment" & ASCII.LF & "--",
         Insert         => "",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Delete_Comment_Start_04;

   procedure Delete_Comment_Start_05 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  This one deletes part of a token, all of a second token, all of a
      --  following comment, and the comment start of the second comment.
      Parse_Text
        (Label   => "3",
         Initial => "A := B_1 + " & ASCII.LF &
           --        |1       |10
           "-- comment" & ASCII.LF &
           --  |15        |22
           "-- + ada_identifier;" & ASCII.LF &
           --      |30
           "C;",

         Edit_At        => 7,
         Delete         => "_1 + " & ASCII.LF & "-- comment" & ASCII.LF & "--",
         Insert         => "",
         Initial_Errors => 0,
         Incr_Errors    => 0);

      --  Edited source:
      --  "A := B + ada_identifier;" & ASCII.LF & "C;"
      --   |1       |10       |20      |25
   end Delete_Comment_Start_05;

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

   procedure Insert_Comment_Start_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert a comment start; from ada_mode-interactive_02.adb
      Parse_Text
        (Initial => "A := B;" & ASCII.LF & " C := D;" & ASCII.LF & " E := F;",
         --          |1         |8           |10        |17
         Edit_At => 9,
         Delete  => "",
         Insert  => "-- ");
   end Insert_Comment_Start_01;

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
         Tree : WisiToken.Syntax_Trees.Tree renames Ada_Lite.Incremental_Parser.Tree;
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

         Edit_At        => 34,
         Delete         => "",
         Insert         => " Name",
         Initial_Errors => 1,
         Incr_Errors    => 0);

   end Missing_Name_1;

   procedure Recover_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
         Initial_Errors => 1,
         Incr_Errors => 0);
   end Recover_01;

   procedure Recover_02 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
           "procedure Pkg.Proc_1 is begin A; begin B; end Pkg.Proc_1;",
         --          |10       |20       |30       |40       |50
         Edit_At           => 43,
         Delete            => "",
         Insert            => "end; ",
         Initial_Errors => 1,
         Incr_Errors => 0);
   end Recover_02;

   procedure Recover_03 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  LR1 incremental parse encountered an invalid sequential index
      --  error in error recovery; now fixed.

      Parse_Text
        (Initial        =>
           "procedure Debug is A : Integer := 4; end Debug;",
         --          |10       |20       |30       |40
         Edit_At        => 15,
         Delete         => "g",
         Insert         => "",
         Initial_Errors => 1,
         Incr_Errors    => 1);
   end Recover_03;

   procedure Recover_04 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  This encountered a bug in recover Left_Breakdown. From
      --  wisitoken_grammar_mode recover_01.wy.

      Incremental_Parser := Grammar.Incremental_Parser'Access;
      Full_Parser        := Grammar.Full_Parser'Access;

      Parse_Text
        (Label   => "1",
         Initial =>
           "range_attribute_designator : 'range' '(' expression ')' ;" & ASCII.LF &
             --      |10       |20       |30       |40       |50         |58
             "aggregate : record_aggregate" & ASCII.LF &
             --  |62     |70       |80
             " | extension_aggregate ;" & ASCII.LF &
             --  |91      |100      |110
             "record_aggregate : '(' record_component_association_list ')' ;",
         --   |113

         Edit_At        => 113,
         Delete         => "",
         Insert         => ";; ",
         Initial_Errors => 0,
         Incr_Errors    => 0);

      Parse_Text
        (Label          => "2",
         Initial        => "",
         Edit_At        => 69,
         Delete         => "",
         Insert         => ";",
         Initial_Errors => 1,
         Incr_Errors    => 1);

      Parse_Text
        (Label          => "3",
         Initial        => "",
         Edit_At        => 70,
         Delete         => "",
         Insert         => ";",
         Initial_Errors => 1,
         Incr_Errors    => 1);

      Parse_Text
        (Label          => "4",
         Initial        => "",
         Edit_At        => 71,
         Delete         => "",
         Insert         => " ",
         Initial_Errors => 1,
         Incr_Errors    => 1);
   end Recover_04;

   procedure Recover_05a (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_05a .. Recover_05e test all cases of n items in
      --  optimized_list before/after edit point; list has a separator.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "procedure a (p1 : Int; p2 : int; p3 : int; p4 : int; p5 : int); procedure B; procedure C;",
         --                   |10       |20
         Edit_At        => 14,
         Delete         => "",
         Insert         => "  ",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Recover_05a;

   procedure Recover_05b (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_05a .. Recover_05e test all cases of n items in
      --  optimized_list before/after edit point; list has a separator.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "procedure a (p1 : Int; p2 : int; p3 : int; p4 : int; p5 : int); procedure B; procedure C;",
         --                   |10       |20
         Edit_At        => 24,
         Delete         => "",
         Insert         => "  ",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Recover_05b;

   procedure Recover_05c (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_05a .. Recover_05e test all cases of n items in
      --  optimized_list before/after edit point; list has a separator.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "procedure a (p1 : Int; p2 : int; p3 : int; p4 : int; p5 : int); procedure B; procedure C;",
         --                   |10       |20       |30
         Edit_At        => 34,
         Delete         => "",
         Insert         => "  ",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Recover_05c;

   procedure Recover_05d (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_05a .. Recover_05e test all cases of n items in
      --  optimized_list before/after edit point; list has a separator.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "procedure a (p1 : Int; p2 : int; p3 : int; p4 : int; p5 : int); procedure B; procedure C;",
         --                   |10       |20       |30       |40
         Edit_At        => 44,
         Delete         => "",
         Insert         => "  ",
         Initial_Errors => 0,
         Incr_Errors    => 0);

   end Recover_05d;

   procedure Recover_05e (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_05a .. Recover_05e test all cases of n items in
      --  optimized_list before/after edit point when list has a separator.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "procedure a (p1 : Int; p2 : int; p3 : int; p4 : int; p5 : int); procedure B; procedure C;",
         --                   |10       |20       |30       |40       |50
         Edit_At        => 54,
         Delete         => "",
         Insert         => "  ",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Recover_05e;

   procedure Recover_06a (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_06a .. Recover_06e test all cases of n items in
      --  optimized_list before/after edit point when breakdown target ID
      --  is the list ID.
      --
      --  The missing statement in the procedure bodies labels the
      --  subprogram body declaration node a "recover_conflict", so
      --  Edit_Tree calls Breakdown with that node as the target;
      --  declarations are an optimized list.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "package body AMIR1 is" & ASCII.LF &
           --                 |10       |20
           "procedure a is begin end a;" & ASCII.LF &
           --      |30
           "procedure b;" & ASCII.LF &
           "procedure c;" & ASCII.LF &
           "procedure d;" & ASCII.LF &
           "procedure e;" & ASCII.LF &
           "end AMIR1;",

         Edit_At        => 34,
         Delete         => " is begin end a;",
         Insert         => ";",
         Initial_Errors => 1,
         Incr_Errors    => 0,
         Optimized_List => True);
   end Recover_06a;

   procedure Recover_06b (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_06a .. Recover_06e test all cases of n items in
      --  optimized_list before/after edit point when breakdown target ID
      --  is the list ID.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "package body AMIR1 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --      |30
           "procedure b is begin end b;" & ASCII.LF &
           --   |40
           "procedure c;" & ASCII.LF &
           "procedure d;" & ASCII.LF &
           "procedure e;" & ASCII.LF &
           "end AMIR1;",

         Edit_At        => 47,
         Delete         => " is begin end b;",
         Insert         => ";",
         Initial_Errors => 1,
         Incr_Errors    => 0,
         Optimized_List => True);
   end Recover_06b;

   procedure Recover_06c (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_06a .. Recover_06e test all cases of n items in
      --  optimized_list before/after edit point when breakdown target ID
      --  is the list ID.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "package body AMIR1 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --      |30
           "procedure b;" & ASCII.LF &
           --   |40
           "procedure c is begin end c;" & ASCII.LF &
           --          |60
           "procedure d;" & ASCII.LF &
           "procedure e;" & ASCII.LF &
           "end AMIR1;",

         Edit_At        => 60,
         Delete         => " is begin end c;",
         Insert         => ";",
         Initial_Errors => 1,
         Incr_Errors    => 0);
   end Recover_06c;

   procedure Recover_06d (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_06a .. Recover_06e test all cases of n items in
      --  optimized_list before/after edit point when breakdown target ID
      --  is the list ID.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "package body AMIR1 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --      |30
           "procedure b;" & ASCII.LF &
           --   |40
           "procedure c;" & ASCII.LF &
           --          |60
           "procedure d is begin end d;" & ASCII.LF &
           --       |70
           "procedure e;" & ASCII.LF &
           "end AMIR1;",

         Edit_At        => 73,
         Delete         => " is begin end d;",
         Insert         => ";",
         Initial_Errors => 1,
         Incr_Errors    => 0);
   end Recover_06d;

   procedure Recover_06e (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Recover_06a .. Recover_06e test all cases of n items in
      --  optimized_list before/after edit point when breakdown target ID
      --  is the list ID.

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "package body AMIR1 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --      |30
           "procedure b;" & ASCII.LF &
           --   |40
           "procedure c;" & ASCII.LF &
           --          |60
           "procedure d;" & ASCII.LF &
           --       |70
           "procedure e is begin end e;" & ASCII.LF &
           --    |80
           "end AMIR1;",

         Edit_At        => 86,
         Delete         => " is begin end e;",
         Insert         => ";",
         Initial_Errors => 1,
         Incr_Errors    => 0);
   end Recover_06e;

   procedure Recover_07 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Edit an optimized_list that is nested in another, with intervening
      --  non-list nodes; from ada_mode-interactive_02.adb

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Initial => "package body AMIR1 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --      |30
           "procedure b is a : int; begin null; end b;" & ASCII.LF &
           --   |40       |50       |60       |70
           "procedure c;" & ASCII.LF &
           --     |85
           "procedure d is a : int; begin null; end d;" & ASCII.LF &
           --       |100      |110
           "procedure e;" & ASCII.LF &
           "end AMIR1;",
         Edit_At        => 52,
         Delete         => "",
         Insert         => "1",
         Initial_Errors => 0,
         Incr_Errors    => 0,
         Optimized_List => True);
   end Recover_07;

   procedure Recover_08a (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite_Ebnf_Actions;
   begin
      --  Recover_08? cover all cases where Breakdown optimized_list
      --  encounters an RHS_Index = 2 node.

      --  The first edit and breakdown leaves a parse stream like this:
      --  ... procedure ..., error token, ), ;, declaration_list ...
      --
      --  Parsing that produces a tree like:
      --  ...
      --  | declarative_part_0
      --  | | declarations_list_2
      --  | | | declarations_list_0
      --  | | | | ... deleted error token ...
      --  | | | declarations_list_1
      --  | | | | ...

      --  Make (insert 'end') expensive so error recovery chooses (delete ';').
      Ada_Lite_EBNF.Full_Parser.Table.McKenzie_Param.Insert (+END_ID)        := 4;
      Ada_Lite_EBNF.Incremental_Parser.Table.McKenzie_Param.Insert (+END_ID) := 4;

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Label   => "1",
         Initial => "package body AMI3 is" & ASCII.LF &
           --                 |10       |20
           "procedure a (p1 : int);" & ASCII.LF &
           --       |30       |40
           "procedure b;" & ASCII.LF &
           --   |50
           "procedure c (p1 : int);" & ASCII.LF &
           --          |70       |80
           "procedure d;" & ASCII.LF &
           --      |90
           "procedure e;" & ASCII.LF &
           --   |100
           "type f is (C1);" & ASCII.LF &
           --          |120
           "end AMI3;",
         Edit_At        => 43, -- extra ; after int in a
         Delete         => "",
         Insert         => ";",
         Initial_Errors => 0,
         Incr_Errors    => 1,
         Optimized_List => True);

      --  We now have the declarations_list_2 tree.
      --
      --  The edit we do next is not important; the declarations_list_2 node
      --  is encountered when restoring the deleted ';' in a, which does
      --  Breakdown with Target = ) in a. This is handled by the not
      --  optimized_list branch in Breakdown.
      Parse_Text
        (Label   => "2",
         Initial => "",
         Edit_At        => 123, -- edit f enum literal again
         Delete         => "",
         Insert         => "3",
         Initial_Errors => 1,
         Incr_Errors    => 1,
         Optimized_List => True);

   end Recover_08a;

   procedure Recover_08b (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite_Ebnf_Actions;
   begin
      --  Recover_08? cover all cases where Breakdown optimized_list
      --  encounters an RHS_Index = 2 node.

      --  The first few edits and breakdown leave a parse stream like this:
      --  ... declaration, error token, declaration_list, declaration, ...
      --
      --  Parsing that produces a tree like:
      --  ...
      --  | declarations_list_1
      --  | | declarations_list_2
      --  | | | declarations_list_0
      --  | | | | ... deleted error token
      --  | | | declarations_list_1
      --  | | | | ...

      --  Make (insert 'end') expensive so error recovery chooses (delete ';').
      Ada_Lite_EBNF.Full_Parser.Table.McKenzie_Param.Insert (+END_ID)        := 4;
      Ada_Lite_EBNF.Incremental_Parser.Table.McKenzie_Param.Insert (+END_ID) := 4;

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Label   => "1",
         Initial => "package body AMI3 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --       |30
           "procedure b (p1 : int);" & ASCII.LF &
           --    |40       |50
           "procedure c (p1 : int);" & ASCII.LF &
           --          |70       |80
           "procedure d;" & ASCII.LF &
           --      |90
           "procedure e;" & ASCII.LF &
           --   |100
           "type f is (C1);" & ASCII.LF &
           --          |120
           "end AMI3;",
         Edit_At        => 33, -- extra ; after a
         Delete         => "",
         Insert         => ";",
         Initial_Errors => 0,
         Incr_Errors    => 1,
         Optimized_List => True);

      Parse_Text
        (Label   => "2",
         Initial => "",
         Edit_At        => 122, -- edit f enum literal
         Delete         => "",
         Insert         => "2",
         Initial_Errors => 1,
         Incr_Errors    => 1,
         Optimized_List => True);

      --  We now have a tree like:
      --  ...
      --  | declarations_list_1
      --  | | declarations_list_2
      --  | | | declarations_list_0
      --  | | | | a. deleted ;
      --  | | | declarations_list_1
      --  | | | | b c d e
      --  ...

      --  The edit we do next is not important; the declarations_list_2 node
      --  is encountered when restoring the deleted ';' after a, which does
      --  Breakdown with Target = first terminal of b, which is in the
      --  second child of the declarations_list_2
      Parse_Text
        (Label   => "3",
         Initial => "",
         Edit_At        => 123, -- edit f enum literal again
         Delete         => "",
         Insert         => "3",
         Initial_Errors => 1,
         Incr_Errors    => 1,
         Optimized_List => True);

   end Recover_08b;

   procedure Recover_08c (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite_Ebnf_Actions;
   begin
      --  Recover_08? cover all cases where Breakdown optimized_list
      --  encounters an RHS_Index = 2 node.

      --  Make (insert 'end') expensive so error recovery chooses (delete ';').
      Ada_Lite_EBNF.Full_Parser.Table.McKenzie_Param.Insert (+END_ID)        := 4;
      Ada_Lite_EBNF.Incremental_Parser.Table.McKenzie_Param.Insert (+END_ID) := 4;

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Label   => "1",
         Initial => "package body AMI3 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --       |30
           "procedure b (p1 : int);" & ASCII.LF &
           --    |40       |50
           "procedure c (p1 : int);" & ASCII.LF &
           --          |70       |80
           "procedure d;" & ASCII.LF &
           --      |90
           "procedure e;" & ASCII.LF &
           --   |100
           "type f is (C1);" & ASCII.LF &
           --          |120
           "end AMI3;",
         Edit_At        => 56, -- extra ; in b param_list
         Delete         => "",
         Insert         => ";",
         Initial_Errors => 0,
         Incr_Errors    => 1,
         Optimized_List => True);

      Parse_Text
        (Label   => "2",
         Initial => "",
         Edit_At        => 122, -- edit f enum literal
         Delete         => "",
         Insert         => "2",
         Initial_Errors => 1,
         Incr_Errors    => 1,
         Optimized_List => True);

      --  We now have a tree like:
      --  ...
      --  | declarations_list_1
      --  | | declarations_list_2
      --  | | | declarations_list_1
      --  | | | | a b deleted ;)
      --  | | | declarations_list_1
      --  | | | | c d e
      --  ...

      --  The breakdown target is the closing paren in b, which is in the
      --  first child of the declarations_list_2 node.
      Parse_Text
        (Label   => "3",
         Initial => "",
         Edit_At        => 123, -- edit f enum literal again
         Delete         => "",
         Insert         => "3",
         Initial_Errors => 1,
         Incr_Errors    => 1,
         Optimized_List => True);

   end Recover_08c;

   procedure Recover_08d (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite_Ebnf_Actions;
   begin
      --  Recover_08? cover all cases where Breakdown optimized_list
      --  encounters an RHS_Index = 2 node.

      --  The first few edits and breakdown leave a parse stream like this:
      --  ... declaration_list, error token, declaration_list, declaration, ...
      --
      --  Parsing that produces a tree like:
      --  ...
      --  | declarations_list_1
      --  | | declarations_list_2
      --  | | | declarations_list_1
      --  | | | | ... deleted error token
      --  | | | declarations_list_1
      --  | | | | ...

      --  Make (insert 'end') expensive so error recovery chooses (delete ';').
      Ada_Lite_EBNF.Full_Parser.Table.McKenzie_Param.Insert (+END_ID)        := 4;
      Ada_Lite_EBNF.Incremental_Parser.Table.McKenzie_Param.Insert (+END_ID) := 4;

      Incremental_Parser := Ada_Lite_EBNF.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite_EBNF.Full_Parser'Access;

      Parse_Text
        (Label   => "1",
         Initial => "package body AMI3 is" & ASCII.LF &
           --                 |10       |20
           "procedure a;" & ASCII.LF &
           --       |30
           "procedure b (p1 : int);" & ASCII.LF &
           --    |40       |50
           "procedure c (p1 : int);" & ASCII.LF &
           --          |70       |80
           "procedure d;" & ASCII.LF &
           --      |90
           "procedure e;" & ASCII.LF &
           --   |100
           "type f is (C1);" & ASCII.LF &
           --          |120
           "end AMI3;",
         Edit_At        => 81, -- extra ; after c
         Delete         => "",
         Insert         => ";",
         Initial_Errors => 0,
         Incr_Errors    => 1,
         Optimized_List => True);

      Parse_Text
        (Label   => "2",
         Initial => "",
         Edit_At        => 122, -- edit f enum literal
         Delete         => "",
         Insert         => "2",
         Initial_Errors => 1,
         Incr_Errors    => 1,
         Optimized_List => True);

      --  We now have a tree like:
      --  ...
      --  | declarations_list_1
      --  | | declarations_list_2
      --  | | | declarations_list_1
      --  | | | | a b c, deleted ;
      --  | | | declarations_list_1
      --  | | | | d e
      --  ...

      --  This is essentially the same as Recover_08b, and is handled by the
      --  same branch in Breakdown.
      Parse_Text
        (Label   => "3",
         Initial => "",
         Edit_At        => 123, -- edit f enum literal again
         Delete         => "",
         Insert         => "3",
         Initial_Errors => 1,
         Incr_Errors    => 1,
         Optimized_List => True);

   end Recover_08d;

   procedure Lexer_Errors_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
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
         Initial_Errors => 2,  --  Lexer + parser.
         Incr_Errors => 2); --  Errors are still in tree
   end Lexer_Errors_01;

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
         Initial_Errors => 1,
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
         Initial_Errors => 1,
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
         Initial_Errors => 0,
         Incr_Errors => 1);

      declare
         use WisiToken;
         use WisiToken.Syntax_Trees;
         use AUnit.Checks;
         Tree : Syntax_Trees.Tree renames Ada_Lite.Incremental_Parser.Tree;
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
         Initial_Errors => 1,
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
         Edit_At        => 27,
         Delete         => "",
         Insert         => ";",
         Edit_2_At      => 54,
         Delete_2       => "",
         Insert_2       => " end C; begin null;",
         Initial_Errors => 2,
         Incr_Errors    => 0);

      --  Edited text:
      --  "procedure A is B : Integer;" & ASCII.LF &
      --   |1       |10       |20
      --  "procedure C is begin null; end C; begin null; end A; procedure D is begin null; end D;",
      --   |29        |40       |50       |60       |70       |80       |90       |100      |110

      --  There are two errors; missing ';' after 'Integer',
      --  missing_name_error on 'procedure C'. The solution to the
      --  match_names_error requires unreducing subprogram_body C, so both
      --  errors store an error on 'procedure', which are cleared by Edit_Tree.
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
         Initial_Errors => 2,
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
         Initial_Errors => 0,
         Incr_Errors => 0);
   end Non_Ascii;

   procedure Restore_Deleted_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Simplified from ada_mode-interactive_09.adb

      Parse_Text
        ("procedure A is begin" & ASCII.LF &
           --  |6  |10       |20  |21
           "   for" & ASCII.LF &
           --  |25    |28
           "end A;",
         Edit_At        => 28,
         Delete         => "",
         Insert         => " ",
         Initial_Errors => 1,
         Incr_Errors    => 1);
   end Restore_Deleted_01;

   procedure Nonterm_Resume_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Test that resume_active is computed properly when the error is on a nonterm.

      Parse_Text
        ("procedure A is b : Int;" &
           --      |10       |20
           " Tree Tree : WisiToken.Syntax_Trees.Tree := C; begin null; end A;",
         --  |25  |30       |40       |50       |60       |70
         Edit_At        => 28,
         Delete         => "",
         Insert         => "e",
         Initial_Errors => 1,
         Incr_Errors    => 1);
   end Nonterm_Resume_01;

   procedure Undo_Conflict_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Requires Edit_Tree to breakdown nonterms marked Recover_Conflict.
      --  Simplified from ada_mode-interactive_15.adb.

      Parse_Text
        ("procedure A is P : S; C : N begin null; end A;",
         --   |5   |10       |20       |30
         Edit_At        => 29,
         Delete         => "",
         Insert         => "renames B; ",
         Initial_Errors => 1,
         Incr_Errors    => 0);
   end Undo_Conflict_01;

   --  See also Lexer_Error_01 for an additional string case.
   procedure Edit_String_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete the start string quote of an existing string literal. New
      --  string literal created by trailing quote is terminated by
      --  new-line.

      Parse_Text
        ("procedure A is B : S := ""123"";" & ASCII.LF &
           --      |10       |20         |30
           "C : S := ""ABC"";" & ASCII.LF &
           --  |35   |41    |47
           "begin null; end A;",
         Edit_At        => 25,
         Delete         => """",
         Insert         => "",
         Initial_Errors => 0,
         Incr_Errors    => 2); -- lexer and syntax errors
   end Edit_String_01;

   procedure Edit_String_02 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete the end string quote of an existing string literal; string
      --  literal now terminated by new-line.

      Parse_Text
        ("procedure A is B : S := ""123"";" & ASCII.LF &
           --      |10       |20         |30
           "C : S := ""ABC"";" & ASCII.LF &
           --  |35   |41    |47
           "begin null; end A;",
         Edit_At        => 29,
         Delete         => """",
         Insert         => "",
         Initial_Errors => 0,
         Incr_Errors    => 2); -- lexer and syntax errors
   end Edit_String_02;

   procedure Edit_String_03 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete the start string quote of an existing string literal. New
      --  string literal created by trailing quote is terminated by next
      --  string literal start delimiter, leaving a dangling string start.
      --  This requires that deleting a string terminator always lex thru
      --  the end of line.

      Parse_Text
        ("procedure A is B : S := ""123""; C : S := ""ABC"";" & ASCII.LF &
           --      |10       |20         |30       |40
           "begin null; end A;",
         Edit_At        => 25,
         Delete         => """",
         Insert         => "",
         Initial_Errors => 0,
         Incr_Errors    => 2); -- lexer and syntax errors
   end Edit_String_03;

   procedure Edit_String_04 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Delete the end string quote of an existing string literal; it is
      --  then terminated by the next string literal start delimiter,
      --  leaving a dangling string start. This requires that deleting a
      --  string terminator always lex thru the end of line.

      Parse_Text
        ("procedure A is B : S := ""123""; C : S := ""ABC"";" & ASCII.LF &
           --      |10       |20         |30       |40
           "begin null; end A;",
         Edit_At        => 29,
         Delete         => """",
         Insert         => "",
         Initial_Errors => 0,
         Incr_Errors    => 2); -- lexer and syntax errors
   end Edit_String_04;

   procedure Edit_String_05 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert a string quote immediately before a new-line to start a
      --  desired string literal.

      Parse_Text
        ("procedure A is B : S :=" & ASCII.LF &
           --      |10       |20     |24
           "C : S := ""ABC"";" & ASCII.LF &
           "begin null; end A;",
         Edit_At        => 24,
         Delete         => "",
         Insert         => " ""123",
         Initial_Errors => 1, -- missing expression
         Incr_Errors    => 2); -- 1 lexer and 2 syntax errors. FIXME: edited full parse gets 2
   end Edit_String_05;

   procedure Edit_String_06 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Insert the end string quote to complete a desired string literal.

      Parse_Text
        ("procedure A is B : S := ""123" & ASCII.LF &
           --      |10       |20           |29
           "C : S := ""ABC"";" & ASCII.LF &
           "begin null; end A;",
         Edit_At        => 29,
         Delete         => "",
         Insert         => """;",
         Initial_Errors => 2,
         Incr_Errors    => 0);
   end Edit_String_06;

   procedure Edit_String_07 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Another insert end delimiter case; requires Delayed_Scan.

      Parse_Text
        ("procedure A is B : S := ""123456"";" & ASCII.LF &
           --      |10       |20         |30
           "begin null; end A;",
         Edit_At        => 27,
         Delete         => "",
         Insert         => "a",
         Edit_2_At      => 29,
         Delete_2       => "",
         Insert_2       => "b"" & """,
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Edit_String_07;

   procedure Edit_String_08 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Another Delayed_Scan case; inserted new_line complicates
      --  Find_Scan_End.

      Parse_Text
        ("procedure A is B : S := ""123456"";" & ASCII.LF &
           --      |10       |20         |30
           "begin null; end A;",
         Edit_At        => 27,
         Delete         => "",
         Insert         => "a",
         Edit_2_At      => 29,
         Delete_2       => "",
         Insert_2       => "b"" &" & ASCII.LF & """",
         Initial_Errors => 0,
         Incr_Errors    => 0);
   end Edit_String_08;

   procedure Edit_String_09 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  The edit affects a string_literal with a lexer error.

      Parse_Text
        ("procedure A is B : S := ""123;" & ASCII.LF &
           --      |10       |20            |30
           "begin null; end A;",
         Edit_At        => 25,
         Delete         => """",
         Insert         => "",
         Initial_Errors => 2,
         Incr_Errors    => 0);
   end Edit_String_09;

   procedure Edit_String_10 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  precondition fail on lexer.Line_Begin_Char_Pos

      Parse_Text
        ("procedure A is begin A := 123;" & ASCII.LF &
         --   |5   |10       |20       |30
           "begin null; end end A;",
         --    |35  |40
         Edit_At        => 27,
         Delete         => "",
         Insert         => """",
         Initial_Errors => 1,
         Incr_Errors    => 3);
   end Edit_String_10;

   procedure Edit_String_11 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
   begin
      --  Used to get "error in resume"

      Parse_Text
        ("procedure Ada_Mode.Recover_String_Quote_5 is begin Test_One; end Ada_Mode.Recover_String_Quote_5;" &
           --  |6  |10       |20       |30       |40       |50       |60       |70       |80
           ASCII.LF,
         Edit_At        => 80,
         Delete         => "",
         Insert         => """",
         Initial_Errors => 0,
         Incr_Errors    => 2);
   end Edit_String_11;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, No_Change'Access, "No_Change");
      Register_Routine (T, Edit_Comment'Access, "Edit_Comment");
      Register_Routine (T, Edit_Comment_02'Access, "Edit_Comment_02");
      Register_Routine (T, Edit_Comment_03'Access, "Edit_Comment_03");
      Register_Routine (T, Edit_Comment_04'Access, "Edit_Comment_04");
      Register_Routine (T, Edit_Comment_05'Access, "Edit_Comment_05");
      Register_Routine (T, Edit_Comment_06'Access, "Edit_Comment_06");
      Register_Routine (T, Edit_Comment_07'Access, "Edit_Comment_07");
      Register_Routine (T, Edit_Comment_08'Access, "Edit_Comment_08");
      Register_Routine (T, Edit_Comment_09'Access, "Edit_Comment_09");
      Register_Routine (T, Edit_Comment_10'Access, "Edit_Comment_10");
      Register_Routine (T, Edit_Comment_11'Access, "Edit_Comment_11");
      Register_Routine (T, Edit_Comment_12'Access, "Edit_Comment_12");
      Register_Routine (T, Edit_Comment_13'Access, "Edit_Comment_13");
      Register_Routine (T, Edit_Comment_14'Access, "Edit_Comment_14");
      Register_Routine (T, Edit_Comment_15'Access, "Edit_Comment_15");
      Register_Routine (T, Edit_Whitespace_1'Access, "Edit_Whitespace_1");
      Register_Routine (T, Edit_Whitespace_2'Access, "Edit_Whitespace_2");
      Register_Routine (T, Edit_Whitespace_3'Access, "Edit_Whitespace_3");
      Register_Routine (T, Edit_Leading_Non_Grammar'Access, "Edit_Leading_Non_Grammar");
      Register_Routine (T, Edit_Code_01'Access, "Edit_Code_01");
      Register_Routine (T, Edit_Code_02'Access, "Edit_Code_02");
      Register_Routine (T, Edit_Code_03'Access, "Edit_Code_03");
      Register_Routine (T, Edit_Code_04'Access, "Edit_Code_04");
      Register_Routine (T, Edit_Code_05'Access, "Edit_Code_05");
      Register_Routine (T, Edit_Code_06'Access, "Edit_Code_06");
      Register_Routine (T, Edit_Code_07'Access, "Edit_Code_07");
      Register_Routine (T, Edit_Code_08'Access, "Edit_Code_08");
      Register_Routine (T, Edit_Code_09'Access, "Edit_Code_09");
      Register_Routine (T, Edit_Code_10'Access, "Edit_Code_10");
      Register_Routine (T, Edit_Code_11'Access, "Edit_Code_11");
      Register_Routine (T, Edit_Code_12'Access, "Edit_Code_12");
      Register_Routine (T, Edit_Code_13'Access, "Edit_Code_13");
      Register_Routine (T, Edit_Code_14'Access, "Edit_Code_14");
      Register_Routine (T, Edit_Code_15'Access, "Edit_Code_15");
      Register_Routine (T, Edit_Code_16'Access, "Edit_Code_16");
      Register_Routine (T, Edit_Code_17'Access, "Edit_Code_17");
      Register_Routine (T, Delete_New_Line'Access, "Delete_New_Line");
      Register_Routine (T, Delete_Comment_End'Access, "Delete_Comment_End");
      Register_Routine (T, Delete_Comment_Start_01'Access, "Delete_Comment_Start_01");
      Register_Routine (T, Delete_Comment_Start_02'Access, "Delete_Comment_Start_02");
      Register_Routine (T, Delete_Comment_Start_03'Access, "Delete_Comment_Start_03");
      Register_Routine (T, Delete_Comment_Start_04'Access, "Delete_Comment_Start_04");
      Register_Routine (T, Delete_Comment_Start_05'Access, "Delete_Comment_Start_05");
      Register_Routine (T, Insert_New_Line'Access, "Insert_New_Line");
      Register_Routine (T, Insert_Comment_Start_01'Access, "Insert_Comment_Start_01");
      Register_Routine (T, Names'Access, "Names");
      Register_Routine (T, Missing_Name_1'Access, "Missing_Name_1");
      Register_Routine (T, Recover_01'Access, "Recover_01");
      Register_Routine (T, Recover_02'Access, "Recover_02");
      Register_Routine (T, Recover_03'Access, "Recover_03");
      Register_Routine (T, Recover_04'Access, "Recover_04");
      Register_Routine (T, Recover_05a'Access, "Recover_05a");
      Register_Routine (T, Recover_05b'Access, "Recover_05b");
      Register_Routine (T, Recover_05c'Access, "Recover_05c");
      Register_Routine (T, Recover_05d'Access, "Recover_05d");
      Register_Routine (T, Recover_05e'Access, "Recover_05e");
      Register_Routine (T, Recover_06a'Access, "Recover_06a");
      Register_Routine (T, Recover_06b'Access, "Recover_06b");
      Register_Routine (T, Recover_06c'Access, "Recover_06c");
      Register_Routine (T, Recover_06d'Access, "Recover_06d");
      Register_Routine (T, Recover_06e'Access, "Recover_06e");
      Register_Routine (T, Recover_07'Access, "Recover_07");
      Register_Routine (T, Recover_08a'Access, "Recover_08a");
      Register_Routine (T, Recover_08b'Access, "Recover_08b");
      Register_Routine (T, Recover_08c'Access, "Recover_08c");
      Register_Routine (T, Recover_08d'Access, "Recover_08d");
      Register_Routine (T, Lexer_Errors_01'Access, "Lexer_Errors_01");
      Register_Routine (T, Preserve_Parse_Errors_1'Access, "Preserve_Parse_Errors_1");
      Register_Routine (T, Preserve_Parse_Errors_2'Access, "Preserve_Parse_Errors_2");
      Register_Routine (T, Modify_Deleted_Node'Access, "Modify_Deleted_Node");
      Register_Routine (T, Multiple_Errors_On_One_Token_1'Access, "Multiple_Errors_On_One_Token_1");
      Register_Routine (T, Multiple_Errors_On_One_Token_2'Access, "Multiple_Errors_On_One_Token_2");
      Register_Routine (T, Non_Ascii'Access, "Non_Ascii");
      Register_Routine (T, Restore_Deleted_01'Access, "Restore_Deleted_01");
      Register_Routine (T, Nonterm_Resume_01'Access, "Nonterm_Resume_01");
      Register_Routine (T, Undo_Conflict_01'Access, "Undo_Conflict_01");
      Register_Routine (T, Edit_String_01'Access, "Edit_String_01");
      Register_Routine (T, Edit_String_02'Access, "Edit_String_02");
      Register_Routine (T, Edit_String_03'Access, "Edit_String_03");
      Register_Routine (T, Edit_String_04'Access, "Edit_String_04");
      Register_Routine (T, Edit_String_05'Access, "Edit_String_05");
      Register_Routine (T, Edit_String_06'Access, "Edit_String_06");
      Register_Routine (T, Edit_String_07'Access, "Edit_String_07");
      Register_Routine (T, Edit_String_08'Access, "Edit_String_08");
      Register_Routine (T, Edit_String_09'Access, "Edit_String_09");
      Register_Routine (T, Edit_String_10'Access, "Edit_String_10");
      Register_Routine (T, Edit_String_11'Access, "Edit_String_11");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_incremental.adb");
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is begin
      --  Run before Register_Tests
      WisiToken.Parse.LR.Parser.New_Parser
        (Ada_Lite.Full_Parser,
         Ada_Lite_LR1_T1_Main.Create_Lexer (Trace'Access),
         Ada_Lite_LR1_T1_Main.Create_Parse_Table
           (Text_Rep_File_Name          => "ada_lite_lr1_t1_re2c_parse_table.txt"),
         Ada_Lite_LR1_T1_Main.Create_Productions,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      WisiToken.Parse.LR.Parser.New_Parser
        (Ada_Lite.Incremental_Parser,
         Ada_Lite_LR1_T1_Main.Create_Lexer (Trace'Access),
         Ada_Lite_LR1_T1_Main.Create_Parse_Table
           (Text_Rep_File_Name          => "ada_lite_lr1_t1_re2c_parse_table.txt"),
         Ada_Lite_LR1_T1_Main.Create_Productions,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      if T.McKenzie_Config /= null then
         WisiToken.Parse.LR.Set_McKenzie_Options
           (Ada_Lite.Incremental_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
         WisiToken.Parse.LR.Set_McKenzie_Options (Ada_Lite.Full_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
      end if;

      Ada_Lite.Orig_McKenzie_Param := Ada_Lite.Full_Parser.Table.McKenzie_Param;

      WisiToken.Parse.LR.Parser.New_Parser
        (Ada_Lite_EBNF.Full_Parser,
         Ada_Lite_Ebnf_LALR_Main.Create_Lexer (Trace'Access),
         Ada_Lite_Ebnf_LALR_Main.Create_Parse_Table,
         Ada_Lite_Ebnf_LALR_Main.Create_Productions,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite_Ebnf.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite_Ebnf.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite_Ebnf.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      WisiToken.Parse.LR.Parser.New_Parser
        (Ada_Lite_EBNF.Incremental_Parser,
         Ada_Lite_Ebnf_LALR_Main.Create_Lexer (Trace'Access),
         Ada_Lite_Ebnf_LALR_Main.Create_Parse_Table,
         Ada_Lite_Ebnf_LALR_Main.Create_Productions,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite_Ebnf.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite_Ebnf.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite_Ebnf.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      if T.McKenzie_Config /= null then
         WisiToken.Parse.LR.Set_McKenzie_Options
           (Ada_Lite_EBNF.Incremental_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
         WisiToken.Parse.LR.Set_McKenzie_Options
           (Ada_Lite_EBNF.Full_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
      end if;

      Ada_Lite_EBNF.Orig_McKenzie_Param := Ada_Lite_EBNF.Full_Parser.Table.McKenzie_Param;

      WisiToken.Parse.LR.Parser.New_Parser
        (Grammar.Full_Parser,
         Grammar_Grammar_01_LR1_T1_Main.Create_Lexer (Trace'Access),
         Grammar_Grammar_01_LR1_T1_Main.Create_Parse_Table,
         Grammar_Grammar_01_LR1_T1_Main.Create_Productions,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Grammar_Grammar_01.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Grammar_Grammar_01.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Grammar_Grammar_01.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      WisiToken.Parse.LR.Parser.New_Parser
        (Grammar.Incremental_Parser,
         Grammar_Grammar_01_LR1_T1_Main.Create_Lexer (Trace'Access),
         Grammar_Grammar_01_LR1_T1_Main.Create_Parse_Table,
         Grammar_Grammar_01_LR1_T1_Main.Create_Productions,
         Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Grammar_Grammar_01.Fixes'Access,
         Language_Matching_Begin_Tokens =>
           WisiToken.Parse.LR.McKenzie_Recover.Grammar_Grammar_01.Matching_Begin_Tokens'Access,
         Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Grammar_Grammar_01.String_ID_Set'Access,
         User_Data                      => User_Data'Access);

      if T.McKenzie_Config /= null then
         WisiToken.Parse.LR.Set_McKenzie_Options
           (Grammar.Incremental_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
         WisiToken.Parse.LR.Set_McKenzie_Options (Grammar.Full_Parser.Table.McKenzie_Param, T.McKenzie_Config.all);
      end if;

      Grammar.Orig_McKenzie_Param := Grammar.Full_Parser.Table.McKenzie_Param;

   end Set_Up_Case;

   overriding procedure Set_Up (T : in out Test_Case)
   is begin
      --  Run before each test in Register_Tests
      Incremental_Parser := Ada_Lite.Incremental_Parser'Access;
      Full_Parser        := Ada_Lite.Full_Parser'Access;

      Ada_Lite_Actions.End_Name_Optional               := True;
      Ada_Lite.Full_Parser.Table.McKenzie_Param        := Ada_Lite.Orig_McKenzie_Param;
      Ada_Lite.Incremental_Parser.Table.McKenzie_Param := Ada_Lite.Orig_McKenzie_Param;

      Ada_Lite_Ebnf_Actions.End_Name_Optional               := True;
      Ada_Lite_EBNF.Full_Parser.Table.McKenzie_Param        := Ada_Lite_EBNF.Orig_McKenzie_Param;
      Ada_Lite_EBNF.Incremental_Parser.Table.McKenzie_Param := Ada_Lite_EBNF.Orig_McKenzie_Param;

      Grammar.Full_Parser.Table.McKenzie_Param        := Grammar.Orig_McKenzie_Param;
      Grammar.Incremental_Parser.Table.McKenzie_Param := Grammar.Orig_McKenzie_Param;
   end Set_Up;

end Test_Incremental;
--  Local Variables:
--  ada-case-strict: nil
--  End:
