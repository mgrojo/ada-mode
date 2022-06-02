--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2021 - 2022 Stephen Leake.  All Rights Reserved.
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lite_Actions;
with Ada_Lite_LR1_T1_Main;
with Grammar_Grammar_01_Actions;
with Grammar_Grammar_01_LR1_T1_Main;
with Optimized_List_LR1_T1_Main;
with SAL.AUnit;
with Skip_To_Grammar_LALR_Main;
with WisiToken.AUnit;
with WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite;
with WisiToken.Parse.LR.McKenzie_Recover.Optimized_List;
with WisiToken.Parse.Parser;
with WisiToken.Syntax_Trees.AUnit_Public;
with WisiToken.Text_IO_Trace;
package body Test_Syntax_Trees is
   use WisiToken;
   use WisiToken.Syntax_Trees;

   Trace     : aliased WisiToken.Text_IO_Trace.Trace;
   Log_File  : Ada.Text_IO.File_Type;
   User_Data : aliased WisiToken.Syntax_Trees.User_Data_Type;

   Ada_Lite_Parser : WisiToken.Parse.Parser.Parser'Class := Ada_Lite_LR1_T1_Main.Create_Parser
     (Trace'Access,
      Text_Rep_File_Name             => "ada_lite_lr1_t1_re2c_parse_table.txt",
      Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Fixes'Access,
      Language_Matching_Begin_Tokens =>
        WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.Matching_Begin_Tokens'Access,
      Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Ada_Lite.String_ID_Set'Access,
      User_Data                      => User_Data'Access);

   Grammar_Parser : WisiToken.Parse.Parser.Parser'Class := Grammar_Grammar_01_LR1_T1_Main.Create_Parser
     (Trace'Access,
      Language_Fixes                 => null,
      Language_Matching_Begin_Tokens => null,
      Language_String_ID_Set         => null,
      User_Data                      => null);

   Optimized_List_Parser : WisiToken.Parse.Parser.Parser'Class := Optimized_List_LR1_T1_Main.Create_Parser
     (Trace'Access,
      Language_Fixes                 => WisiToken.Parse.LR.McKenzie_Recover.Optimized_List.Fixes'Access,
      Language_Matching_Begin_Tokens =>
        WisiToken.Parse.LR.McKenzie_Recover.Optimized_List.Matching_Begin_Tokens'Access,
      Language_String_ID_Set         => WisiToken.Parse.LR.McKenzie_Recover.Optimized_List.String_ID_Set'Access,
      User_Data                      => null);

   Skip_To_Parser : WisiToken.Parse.Parser.Parser'Class := Skip_To_Grammar_LALR_Main.Create_Parser
     (Trace'Access,
      Text_Rep_File_Name => "skip_to_grammar_lalr_parse_table.txt",
      User_Data          => null);

   function Parse_Text (Parser : in out WisiToken.Parse.Parser.Parser'Class; Text : in String) return Stream_Node_Ref
   --  return wisitoken_accept in parse_stream.
   is begin
      Parser.Tree.Lexer.Reset_With_String (Text);

      Parser.LR_Parse (Log_File);
      Parser.Tree.Start_Edit;

      if WisiToken.Trace_Tests > WisiToken.Detail then
         Put_Line ("tree:");
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

      if Trace_Tests > Detail then
         Put_Line ("tree:");
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

      Ada_Lite_Parser.LR_Parse (Log_File);

      if Trace_Tests > Detail then
         Ada.Text_IO.Put_Line ("tree:");
         Ada_Lite_Parser.Tree.Print_Tree (Non_Grammar => True);
      end if;

      declare
         Tree : WisiToken.Syntax_Trees.Tree renames Ada_Lite_Parser.Tree;
         Node : constant WisiToken.Syntax_Trees.Node_Access := Tree.Find_New_Line
           (Line => 2, Line_Begin_Char_Pos => Begin_Char_Pos);
      begin
         Check ("1 node", Tree.ID (Node), +SEMICOLON_ID);
         Check ("1 begin_char_pos", Begin_Char_Pos, 15);
      end;
   end Find_New_Line_1;

   procedure Find_New_Line_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;
      use Grammar_Grammar_01_Actions;

      Text : constant String :=
        "%generate LALR Ada_Emacs re2c" & ASCII.LF & -- 1
        --        |10       |20           |30
        "%code actions spec post" & ASCII.LF & -- 2
        --        |40       |50
        "%{" & ASCII.LF & -- 3
        --     |57
        "   Active : Boolean;" & ASCII.LF & -- 4
        --  |61      |70
        "   Goal   : Buffer_Pos;" & ASCII.LF & -- 5
        --     |85  |90       |100
        "}%" & ASCII.LF & -- 6
        --     |105
        "%mckenzie_minimal_complete_cost_delta -3" & ASCII.LF; -- 7

      procedure One
        (Label                   : in String;
         Line                    : in WisiToken.Line_Number_Type;
         Expected_ID             : in WisiToken.Token_ID;
         Expected_Begin_Char_Pos : in WisiToken.Buffer_Pos)
      is
         Begin_Char_Pos : WisiToken.Buffer_Pos;
         Tree           : WisiToken.Syntax_Trees.Tree renames Grammar_Parser.Tree;
         Node           : constant WisiToken.Syntax_Trees.Node_Access := Tree.Find_New_Line
           (Line, Line_Begin_Char_Pos => Begin_Char_Pos);
      begin
         Check (Label & ".node", Tree.ID (Node), Expected_ID);
         Check (Label & ".begin_char_pos", Begin_Char_Pos, Expected_Begin_Char_Pos);
      end One;

   begin
      Grammar_Parser.Tree.Lexer.Reset_With_String (Text);

      Grammar_Parser.LR_Parse (Log_File);

      if Trace_Tests > Detail then
         Ada.Text_IO.Put_Line ("tree:");
         Grammar_Parser.Tree.Print_Tree (Non_Grammar => True);
      end if;

      One ("1", 2, +IDENTIFIER_ID, 31);
      One ("2", 4, +RAW_CODE_ID, 58);
      One ("3", 6, +RAW_CODE_ID, 103);
      One ("4", 7, +RAW_CODE_ID, 106);
   end Find_New_Line_2;

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

      Ada_Lite_Parser.LR_Parse (Log_File);

      declare
         Tree : WisiToken.Syntax_Trees.Tree renames Ada_Lite_Parser.Tree;
         Node : constant WisiToken.Syntax_Trees.Node_Access := Tree.Find_Descendant
           (Tree.Root, +parameter_profile_opt_ID);
      begin
         Check ("1 byte_region", Tree.Byte_Region (Node, Trailing_Non_Grammar => False), (12, 11));
      end;
   end Byte_Region_1;

   procedure Line_At_Byte_Pos_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      Text : constant String :=
        "-- comment" & ASCII.LF &
        --        |10
        "procedure A" & ASCII.LF & "is begin" & ASCII.LF & "   null;" & ASCII.LF & " end A;";
      --         |20                |24         |32                |40
   begin
      Ada_Lite_Parser.Tree.Lexer.Reset_With_String (Text);

      Ada_Lite_Parser.LR_Parse (Log_File);

      if Trace_Tests > Detail then
         Ada_Lite_Parser.Tree.Print_Tree (Non_Grammar => True);
         Ada_Lite_Parser.Put_Errors;
      end if;

      Check ("1", Ada_Lite_Parser.Tree.Line_At_Byte_Pos (24), 3);
      Check ("2", Ada_Lite_Parser.Tree.Line_At_Byte_Pos (34), 4); --  in whitespace outside non_grammar
      Check ("3", Ada_Lite_Parser.Tree.Line_At_Byte_Pos (1), 1);  --  SOI with leading non_grammar
      Check ("4", Ada_Lite_Parser.Tree.Line_At_Byte_Pos (9), 1);  --  in leading non_grammar
      Check ("5", Ada_Lite_Parser.Tree.Line_At_Byte_Pos (49), 5);  --  EOI
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

      Skip_To_Parser.LR_Parse (Log_File);

      if Trace_Tests > Detail then
         Skip_To_Parser.Tree.Print_Tree (Non_Grammar => True);
      end if;

      Check ("1", Skip_To_Parser.Tree.Line_At_Byte_Pos (13), 1);
      Check ("2", Skip_To_Parser.Tree.Line_At_Byte_Pos (21), 2);
      Check ("3", Skip_To_Parser.Tree.Line_At_Byte_Pos (41), 3);
   end Line_At_Byte_Pos_2;

   procedure Line_At_Byte_Pos_3 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      --  From wisitoken.grammar_mode nominal.wy
      Text : constant String :=
        "%mckenzie_cost_default 2 2 2 2" & ASCII.LF & -- 1
        --        |10       |20       |30
        "extension_aggregate" & ASCII.LF & -- 2
        --       |40       |50
        "  : '(' expression 'with' record ')'" & ASCII.LF & -- 3
        --       |60       |70       |80
        "    %(" & ASCII.LF & -- 4
        --   |93
        "      (wisi-indent-action [nil" & ASCII.LF & -- 5
        --   |100      |110      |120
        "                           (wisi-anchored 1 0)])" & ASCII.LF & -- 6
        --  |130      |140      |150      |160      |170
        "      )%" & ASCII.LF & -- 7
        --   |180
        "  ;"; -- 8
      --   |187
   begin
      Grammar_Parser.Tree.Lexer.Reset_With_String (Text);

      Grammar_Parser.LR_Parse (Log_File);

      if Trace_Tests > Detail then
         Grammar_Parser.Tree.Print_Tree (Non_Grammar => True);
         Grammar_Parser.Put_Errors;
      end if;

      Check ("1", Grammar_Parser.Tree.Line_At_Byte_Pos (1), 1);
      Check ("2", Grammar_Parser.Tree.Line_At_Byte_Pos (10), 1);
      Check ("3", Grammar_Parser.Tree.Line_At_Byte_Pos (31), 1);
      Check ("4", Grammar_Parser.Tree.Line_At_Byte_Pos (32), 2);
      Check ("5", Grammar_Parser.Tree.Line_At_Byte_Pos (90), 4);
      Check ("6", Grammar_Parser.Tree.Line_At_Byte_Pos (93), 4);
      Check ("7", Grammar_Parser.Tree.Line_At_Byte_Pos (100), 5);
      Check ("8", Grammar_Parser.Tree.Line_At_Byte_Pos (160), 6);
      Check ("9", Grammar_Parser.Tree.Line_At_Byte_Pos (180), 7);
      Check ("10", Grammar_Parser.Tree.Line_At_Byte_Pos (185), 8);
      Check ("11", Grammar_Parser.Tree.Line_At_Byte_Pos (187), 8);
      Check ("12", Grammar_Parser.Tree.Line_At_Byte_Pos (188), 8); -- at EOI
      Check ("13", Grammar_Parser.Tree.Line_At_Byte_Pos (189), Invalid_Line_Number); -- after EOI
   end Line_At_Byte_Pos_3;

   procedure Find_Char_Pos_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada_Lite_Actions;

      Text : constant String :=
        "  -- leading comment" & ASCII.LF & "  A; -- comment" & ASCII.LF & "  B;  ";
      --  |2      |10       |20                |24   |30                      |40

      procedure Test_1
        (Label                     : in String;
         Pos                       : in Buffer_Pos;
         Trailing_Non_Grammar      : in Boolean;
         After                     : in Boolean;
         Expected_ID               : in Token_ID;
         Expected_Token_Char_First : in Buffer_Pos)
      is
         use WisiToken.AUnit;

         Tree : WisiToken.Syntax_Trees.Tree renames Ada_Lite_Parser.Tree;
         Node : constant WisiToken.Syntax_Trees.Node_Access := Tree.Find_Char_Pos
           (Pos, Trailing_Non_Grammar, After);
      begin
         if Expected_ID = Invalid_Token_ID then
            WisiToken.Syntax_Trees.AUnit_Public.Check_Address (Label & ".id", Node, null);

         elsif Node = null then
            WisiToken.Syntax_Trees.AUnit_Public.Check_Address (Label & ".id", Node, Tree.SOI);

         else
            Check (Label & ".id", Tree.ID (Node), Expected_ID);
            Check (Label & ".char_first",
                   Tree.Char_Region (Node, Trailing_Non_Grammar => False).First,
                   Expected_Token_Char_First);
         end if;
      end Test_1;
   begin
      Ada_Lite_Parser.Tree.Lexer.Reset_With_String (Text);

      Ada_Lite_Parser.LR_Parse (Log_File);

      --  IMPROVEME: set lexer buffer start after actual text buffer start, test before text.

      Test_1 ("leading whitespace 1", 1, False, False, Invalid_Token_ID, Invalid_Buffer_Pos);
      Test_1 ("leading whitespace 2", 1, True,  False, +Wisi_SOI_ID, 1);
      Test_1 ("leading whitespace 4", 1, False, True,  +IDENTIFIER_ID, 24);

      Test_1 ("leading non_grammar 1", 6, False, False, Invalid_Token_ID, Invalid_Buffer_Pos);
      Test_1 ("leading non_grammar 2", 6, True,  False, +Wisi_SOI_ID, 1);
      Test_1 ("leading non_grammar 4", 6, False, True,  +IDENTIFIER_ID, 24);

      Test_1 ("A leading whitespace 1", 23, False, False, Invalid_Token_ID, Invalid_Buffer_Pos);
      Test_1 ("A leading whitespace 2", 23, True,  False, Invalid_Token_ID, Invalid_Buffer_Pos);
      Test_1 ("A leading whitespace 4", 23, False, True,  +IDENTIFIER_ID, 24);

      Test_1 ("A", 24, False, False, +IDENTIFIER_ID, 24);

      Test_1 ("A trailing whitespace 1", 26, False, False, Invalid_Token_ID, Invalid_Buffer_Pos);
      Test_1 ("A trailing whitespace 2", 26, True,  False, +SEMICOLON_ID, 25);
      Test_1 ("A trailing whitespace 4", 26, False, True,  +IDENTIFIER_ID, 40);

      Test_1 ("B", 40, False, False, +IDENTIFIER_ID, 40);

      Test_1 ("after text 1", 45, True,  False, Invalid_Token_ID, Invalid_Buffer_Pos);
      Test_1 ("after text 2", 45, False, True,  Invalid_Token_ID, Invalid_Buffer_Pos);
   end Find_Char_Pos_1;

   procedure Breakdown_Optimized_List_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Syntax_Trees.AUnit_Public;

      Text : constant String := "a : A; b : B; c : C; d : D; e : E;";
      --  char_pos               |1        |10        |20        |30
      --  node_index             |1     |5     |9     |13    |18

      Tree : WisiToken.Syntax_Trees.Tree renames Optimized_List_Parser.Tree;

      type Expected is record
         Root_Index : Syntax_Trees.Node_Index;
         Depth      : SAL.Base_Peek_Type;
      end record;

      procedure Test_1
        (Label          : in String;
         Index          : in Node_Index;
         Node_Expected  : in Expected;
         Prev_Expected  : in Expected;
         Last_Expected  : in Expected;
         Declarations_0 : in Boolean := True)
      is
         use SAL.AUnit;
         use AUnit.Checks;
      begin
         if Trace_Tests > Detail then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line (Label & " initial parse");
         end if;

         Optimized_List_Parser.Tree.Lexer.Reset_With_String (Text);
         Optimized_List_Parser.LR_Parse (Log_File);

         Tree.Start_Edit;

         declare
            All_Terminals : constant Valid_Node_Access_Array := Tree.Get_Terminals
              (Get_Node (Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True)));

            Ref : Stream_Node_Parents := Tree.To_Stream_Node_Parents
              (Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True));
         begin
            Tree.First_Terminal (Ref, Following => False);
            for I in 1 .. Index - 1 loop
               Tree.Next_Terminal (Ref);
            end loop;
            Check (Label & ".node_index", Tree.Get_Node_Index (Ref.Ref.Node), Index);

            Tree.Breakdown (Ref, Optimized_List_Parser.Productions, null, First_Terminal => True);

            if Trace_Tests > Detail then
               Ada.Text_IO.Put_Line (Label & " edited stream:");
               Tree.Print_Streams (Children => True);
               Ada.Text_IO.Put_Line (Label & "  ref: " & Tree.Image (Ref.Ref, Node_Numbers => True));
            end if;

            declare
               use all type Ada.Containers.Count_Type;
               Error_Reported : Node_Sets.Set;
            begin
               Tree.Validate_Tree
                 (User_Data, Error_Reported,
                  Node_Index_Order => True,
                  Validate_Node    => null);
               --  Mark_In_Tree doesn't work when the tree isn't fully parsed.
               if Error_Reported.Count > 0 then
                  AUnit.Assertions.Assert (False, "invalid tree");
               end if;
            end;

            --  All terminals should still be present.
            declare
               use all type SAL.Base_Peek_Type;

               Term : Terminal_Ref := Tree.First_Terminal (Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True));
               I : Positive_Index_Type := All_Terminals'First;
            begin
               if Trace_Tests > Detail then
                  Ada.Text_IO.Put ("terminals: (");
                  for T of All_Terminals loop
                     Ada.Text_IO.Put (Tree.Image (T, Node_Numbers => True) & ", ");
                  end loop;
                  Ada.Text_IO.Put_Line (")");
               end if;

               loop
                  exit when Term = Invalid_Stream_Node_Ref or else
                    Tree.ID (Term.Node) = Tree.Lexer.Descriptor.EOI_ID;

                  Check_Address (Label & ".terminals (" & Trimmed_Image (I) & ")", Term.Node, All_Terminals (I));

                  I := I + 1;
                  Tree.Next_Terminal (Term);
               end loop;
               if I /= All_Terminals'Last + 1 then
                  AUnit.Assertions.Assert (False, Label & " all_terminals length mismatch");
               end if;
            end;

            declare
               Last : Stream_Node_Parents := Tree.To_Stream_Node_Parents
                 (Tree.Stream_Last (Tree.Shared_Stream, Skip_EOI => True));
               Prev : Stream_Node_Parents := Ref;
            begin
               Tree.Last_Terminal (Last, Tree.Shared_Stream);
               Tree.Prev_Terminal (Prev, Tree.Shared_Stream);

               if Trace_Tests > Detail then
                  Ada.Text_IO.Put_Line (Label & " prev: " & Tree.Image (Prev.Ref, Node_Numbers => True));
                  Ada.Text_IO.Put_Line (Label & " last: " & Tree.Image (Last.Ref, Node_Numbers => True));
               end if;

               Check
                 (Label & ".node.element index",
                  Tree.Get_Node_Index (Get_Node (Ref.Ref.Element)),
                  Node_Expected.Root_Index);
               Check (Label & ".node.depth", Ref.Parents.Depth, Node_Expected.Depth);

               Check
                 (Label & ".prev.element index",
                  Tree.Get_Node_Index (Get_Node (Prev.Ref.Element)),
                  Prev_Expected.Root_Index);
               Check (Label & ".prev_depth", Prev.Parents.Depth, Prev_Expected.Depth);

               Check
                 (Label & ".last.element index",
                  Tree.Get_Node_Index (Get_Node (Last.Ref.Element)),
                  Last_Expected.Root_Index);
               Check (Label & ".last_depth", Last.Parents.Depth, Last_Expected.Depth);
            end;
         end;

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line (Label & " parse edited stream");
         end if;
         Optimized_List_Parser.LR_Parse (Log_File, Pre_Edited => True);

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line (Label & " tree:");
            Tree.Print_Tree;
         end if;
         declare
            use all type Ada.Containers.Count_Type;
            Error_Reported : Node_Sets.Set;
         begin
            Tree.Validate_Tree
              (User_Data, Error_Reported,
               Node_Index_Order => True,
               Validate_Node    => Mark_In_Tree'Access);
            if Error_Reported.Count > 0 then
               AUnit.Assertions.Assert (False, "invalid tree");
            end if;
         end;

         if Declarations_0 then
            Check
              (Label & ".declarations_0 rhs_index", Tree.RHS_Index (Tree.Child (Tree.Root, 2)),
               0);
         end if;
      end Test_1;
   begin
      --  Initial stream is:
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  -33:(wisitoken_accept_0, (1 . 34))
      --  | -32:(declarations_1, (1 . 34))
      --  | | -30:(declarations_1, (1 . 27))
      --  | | | -28:(declarations_1, (1 . 20))
      --  | | | | -26:(declarations_1, (1 . 13))
      --  | | | | | -24:(declarations_0, (1 . 6))
      --  | | | | | | -23:(declaration_0, (1 . 6))
      --  | | | | | | | 1:(IDENTIFIER, (1 . 1))
      --  | | | | | | | 2:(COLON, (3 . 3))
      --  | | | | | | | 3:(IDENTIFIER, (5 . 5))
      --  | | | | | | | 4:(SEMICOLON, (6 . 6))
      --  | | | | | -25:(declaration_0, (8 . 13))
      --  | | | | | | 5:(IDENTIFIER, (8 . 8))
      --  | | | | | | 6:(COLON, (10 . 10))
      --  | | | | | | 7:(IDENTIFIER, (12 . 12))
      --  | | | | | | 8:(SEMICOLON, (13 . 13))
      --  | | | | -27:(declaration_0, (15 . 20))
      --  | | | | | 9:(IDENTIFIER, (15 . 15))
      --  | | | | | 10:(COLON, (17 . 17))
      --  | | | | | 11:(IDENTIFIER, (19 . 19))
      --  | | | | | 12:(SEMICOLON, (20 . 20))
      --  | | | -29:(declaration_0, (22 . 27))
      --  | | | | 13:(IDENTIFIER, (22 . 22))
      --  | | | | 14:(COLON, (24 . 24))
      --  | | | | 15:(IDENTIFIER, (26 . 26))
      --  | | | | 16:(SEMICOLON, (27 . 27))
      --  | | -31:(declaration_0, (29 . 34))
      --  | | | 17:(IDENTIFIER, (29 . 29))
      --  | | | 18:(COLON, (31 . 31))
      --  | | | 19:(IDENTIFIER, (33 . 33))
      --  | | | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))

      Test_1
        (Label          => "a1",     -- stream is unchanged
         Index          => 1,        -- a
         Node_Expected  => (-33, 7), -- a
         Prev_Expected  => (0, 0),   -- SOI
         Last_Expected  => (-33, 3), -- e
         Declarations_0 => False);

      --  "a2" result stream
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  1:(IDENTIFIER, (1 . 1))   a
      --  2:(COLON, (3 . 3))
      --  3:(IDENTIFIER, (5 . 5))
      --  4:(SEMICOLON, (6 . 6))
      --
      --  -38:declarations_1
      --  | -37:declarations_1
      --  | | -36:declarations_1
      --  | | | -35:declarations_1
      --  | | | | -25:(declaration_0, (8 . 13))
      --  | | | | | 5:(IDENTIFIER, (8 . 8))     b
      --  | | | | | 6:(COLON, (10 . 10))
      --  | | | | | 7:(IDENTIFIER, (12 . 12))
      --  | | | | | 8:(SEMICOLON, (13 . 13))
      --  | | | -27:(declaration_0, (15 . 20))
      --  | | | | 9:(IDENTIFIER, (15 . 15))     c
      --  | | | | 10:(COLON, (17 . 17))
      --  | | | | 11:(IDENTIFIER, (19 . 19))
      --  | | | | 12:(SEMICOLON, (20 . 20))
      --  | | -29:(declaration_0, (22 . 27))
      --  | | | 13:(IDENTIFIER, (22 . 22))      d
      --  | | | 14:(COLON, (24 . 24))
      --  | | | 15:(IDENTIFIER, (26 . 26))
      --  | | | 16:(SEMICOLON, (27 . 27))
      --  | -31:(declaration_0, (29 . 34))
      --  | | 17:(IDENTIFIER, (29 . 29))        e
      --  | | 18:(COLON, (31 . 31))
      --  | | 19:(IDENTIFIER, (33 . 33))
      --  | | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))
      Test_1
        ("a2",
         2,                          -- : after a
         Node_Expected => (2, 0),    -- :
         Prev_Expected => (1, 0),    -- a
         Last_Expected => (-38, 2)); -- e

      --  "b1" result stream:
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  -23:(declaration_0, (1 . 6))
      --  | <a : A;>
      --
      --  -25:(declaration_0, (8 . 13))
      --  | 5:(IDENTIFIER, (8 . 8))     b
      --  | 6:(COLON, (10 . 10))
      --  | 7:(IDENTIFIER, (12 . 12))
      --  | 8:(SEMICOLON, (13 . 13))
      --
      --  -37:declarations_1
      --  | -36:declarations_1
      --  | | -35:declarations_0
      --  | | | -27:(declaration_0, (15 . 20))
      --  | | | | 9:(IDENTIFIER, (15 . 15))     c
      --  | | | | 10:(COLON, (17 . 17))
      --  | | | | 11:(IDENTIFIER, (19 . 19))
      --  | | | | 12:(SEMICOLON, (20 . 20))
      --  | | -29:(declaration_0, (22 . 27))
      --  | | | 13:(IDENTIFIER, (22 . 22))      d
      --  | | | 14:(COLON, (24 . 24))
      --  | | | 15:(IDENTIFIER, (26 . 26))
      --  | | | 16:(SEMICOLON, (27 . 27))
      --  | -31:(declaration_0, (29 . 34))
      --  | | 17:(IDENTIFIER, (29 . 29))        e
      --  | | 18:(COLON, (31 . 31))
      --  | | 19:(IDENTIFIER, (33 . 33))
      --  | | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))
      Test_1
        ("b1",
         5,                          -- b
         Node_Expected => (-25, 1),  -- b
         Prev_Expected => (-23, 1),  -- a
         Last_Expected => (-37, 2)); -- e

      --  "b2" result stream:
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  -23:(declaration_0, (1 . 6))
      --  | <a : A;>
      --
      --  5:(IDENTIFIER, (8 . 8))     b
      --
      --  6:(COLON, (10 . 10))
      --
      --  7:(IDENTIFIER, (12 . 12))
      --
      --  8:(SEMICOLON, (13 . 13))
      --
      --  -37:declarations_1
      --  | -36:declarations_1
      --  | | -35:declarations_0
      --  | | | -27:(declaration_0, (15 . 20))
      --  | | | | 9:(IDENTIFIER, (15 . 15))     c
      --  | | | | 10:(COLON, (17 . 17))
      --  | | | | 11:(IDENTIFIER, (19 . 19))
      --  | | | | 12:(SEMICOLON, (20 . 20))
      --  | | -29:(declaration_0, (22 . 27))
      --  | | | 13:(IDENTIFIER, (22 . 22))      d
      --  | | | 14:(COLON, (24 . 24))
      --  | | | 15:(IDENTIFIER, (26 . 26))
      --  | | | 16:(SEMICOLON, (27 . 27))
      --  | -31:(declaration_0, (29 . 34))
      --  | | 17:(IDENTIFIER, (29 . 29))        e
      --  | | 18:(COLON, (31 . 31))
      --  | | 19:(IDENTIFIER, (33 . 33))
      --  | | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))
      Test_1
        ("b2",
         6,                          -- : after b
         Node_Expected => (6, 0),    -- :
         Prev_Expected => (5, 0),    -- b
         Last_Expected => (-37, 2)); -- e

      --  "c1" result stream:
      --  0:SOI
      --
      --  -26:(declarations_1, (1 . 13))
      --  | -24:(declarations_0, (1 . 6))
      --  | | -23:(declaration_0, (1 . 6))
      --  | | | 1:(IDENTIFIER, (1 . 1))   a
      --  | | | 2:(COLON, (3 . 3))
      --  | | | 3:(IDENTIFIER, (5 . 5))
      --  | | | 4:(SEMICOLON, (6 . 6))
      --  | -25:(declaration_0, (8 . 13))
      --  | | 5:(IDENTIFIER, (8 . 8))     b
      --  | | 6:(COLON, (10 . 10))
      --  | | 7:(IDENTIFIER, (12 . 12))
      --  | | 8:(SEMICOLON, (13 . 13))
      --
      --  -27:(declaration_0, (15 . 20))
      --  | 9:(IDENTIFIER, (15 . 15))     c
      --  | 10:(COLON, (17 . 17))
      --  | 11:(IDENTIFIER, (19 . 19))
      --  | 12:(SEMICOLON, (20 . 20))
      --
      --  -36:declarations_1
      --  | -35:declarations_0
      --  | | -29:(declaration_0, (22 . 27))
      --  | | | 13:(IDENTIFIER, (22 . 22))      d
      --  | | | 14:(COLON, (24 . 24))
      --  | | | 15:(IDENTIFIER, (26 . 26))
      --  | | | 16:(SEMICOLON, (27 . 27))
      --  | -31:(declaration_0, (29 . 34))
      --  | | 17:(IDENTIFIER, (29 . 29))        e
      --  | | 18:(COLON, (31 . 31))
      --  | | 19:(IDENTIFIER, (33 . 33))
      --  | | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))

      Test_1
        ("c1",
         9,                          -- c
         Node_Expected => (-27, 1),  -- c
         Prev_Expected => (-26, 2),  -- b
         Last_Expected => (-36, 2)); -- e

      --  "c2" result stream:
      --  0:SOI
      --
      --  -26:(declarations_1, (1 . 13))
      --  | -24:(declarations_0, (1 . 6))
      --  | | -23:(declaration_0, (1 . 6))
      --  | | | 1:(IDENTIFIER, (1 . 1))   a
      --  | | | 2:(COLON, (3 . 3))
      --  | | | 3:(IDENTIFIER, (5 . 5))
      --  | | | 4:(SEMICOLON, (6 . 6))
      --  | -25:(declaration_0, (8 . 13))
      --  | | 5:(IDENTIFIER, (8 . 8))     b
      --  | | 6:(COLON, (10 . 10))
      --  | | 7:(IDENTIFIER, (12 . 12))
      --  | | 8:(SEMICOLON, (13 . 13))
      --
      --  9:(IDENTIFIER, (15 . 15))     c
      --
      --  10:(COLON, (17 . 17))
      --
      --  11:(IDENTIFIER, (19 . 19))
      --
      --  12:(SEMICOLON, (20 . 20))
      --
      --  -36:declarations_1
      --  | -35:declarations_0
      --  | | -29:(declaration_0, (22 . 27))
      --  | | | 13:(IDENTIFIER, (22 . 22))      d
      --  | | | 14:(COLON, (24 . 24))
      --  | | | 15:(IDENTIFIER, (26 . 26))
      --  | | | 16:(SEMICOLON, (27 . 27))
      --  | -31:(declaration_0, (29 . 34))
      --  | | 17:(IDENTIFIER, (29 . 29))        e
      --  | | 18:(COLON, (31 . 31))
      --  | | 19:(IDENTIFIER, (33 . 33))
      --  | | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))

      Test_1
        ("c2",
         10,                         -- : after c
         Node_Expected => (10, 0),   -- :
         Prev_Expected => (9, 0),    -- c
         Last_Expected => (-36, 2)); -- e

      --  "d1" result stream:
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  -28:(declarations_1, (1 . 20))
      --  | -26:(declarations_1, (1 . 13))
      --  | | -24:(declarations_0, (1 . 6))
      --  | | | -23:(declaration_0, (1 . 6))
      --  | | | | 1:(IDENTIFIER, (1 . 1))   a
      --  | | | | 2:(COLON, (3 . 3))
      --  | | | | 3:(IDENTIFIER, (5 . 5))
      --  | | | | 4:(SEMICOLON, (6 . 6))
      --  | | -25:(declaration_0, (8 . 13))
      --  | | | 5:(IDENTIFIER, (8 . 8))     b
      --  | | | 6:(COLON, (10 . 10))
      --  | | | 7:(IDENTIFIER, (12 . 12))
      --  | | | 8:(SEMICOLON, (13 . 13))
      --  | -27:(declaration_0, (15 . 20))
      --  | | 9:(IDENTIFIER, (15 . 15))     c
      --  | | 10:(COLON, (17 . 17))
      --  | | 11:(IDENTIFIER, (19 . 19))
      --  | | 12:(SEMICOLON, (20 . 20))
      --
      --  -29:(declaration_0, (22 . 27))
      --  | 13:(IDENTIFIER, (22 . 22))      d
      --  | 14:(COLON, (24 . 24))
      --  | 15:(IDENTIFIER, (26 . 26))
      --  | 16:(SEMICOLON, (27 . 27))
      --
      --  -31:(declaration_0, (29 . 34))
      --  | 17:(IDENTIFIER, (29 . 29))        e
      --  | 18:(COLON, (31 . 31))
      --  | 19:(IDENTIFIER, (33 . 33))
      --  | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))

      Test_1
        ("d1",
         13,                         -- d
         Node_Expected  => (-29, 1), -- d
         Prev_Expected  => (-28, 2), -- c
         Last_Expected  => (-31, 1), -- e
         Declarations_0 => False);

      --  "d2" result stream:
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  -28:(declarations_1, (1 . 20))
      --  | -26:(declarations_1, (1 . 13))
      --  | | -24:(declarations_0, (1 . 6))
      --  | | | -23:(declaration_0, (1 . 6))
      --  | | | | 1:(IDENTIFIER, (1 . 1))   a
      --  | | | | 2:(COLON, (3 . 3))
      --  | | | | 3:(IDENTIFIER, (5 . 5))
      --  | | | | 4:(SEMICOLON, (6 . 6))
      --  | | -25:(declaration_0, (8 . 13))
      --  | | | 5:(IDENTIFIER, (8 . 8))     b
      --  | | | 6:(COLON, (10 . 10))
      --  | | | 7:(IDENTIFIER, (12 . 12))
      --  | | | 8:(SEMICOLON, (13 . 13))
      --  | -27:(declaration_0, (15 . 20))
      --  | | 9:(IDENTIFIER, (15 . 15))     c
      --  | | 10:(COLON, (17 . 17))
      --  | | 11:(IDENTIFIER, (19 . 19))
      --  | | 12:(SEMICOLON, (20 . 20))
      --
      --  -29:(declaration_0, (22 . 27))
      --  | 13:(IDENTIFIER, (22 . 22))      d
      --  | 14:(COLON, (24 . 24))
      --  | 15:(IDENTIFIER, (26 . 26))
      --  | 16:(SEMICOLON, (27 . 27))
      --
      --  -31:(declaration_0, (29 . 34))
      --  | 17:(IDENTIFIER, (29 . 29))        e
      --  | 18:(COLON, (31 . 31))
      --  | 19:(IDENTIFIER, (33 . 33))
      --  | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))

      Test_1
        ("d2",
         14,                         -- : after d
         Node_Expected => (14, 0),   -- :
         Prev_Expected => (13, 0),   -- d
         Last_Expected => (-31, 1), -- e
         Declarations_0 => False);

      --  "e1" result stream:
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  -30:(declarations_1, (1 . 27))
      --  | -28:(declarations_1, (1 . 20))
      --  | | -26:(declarations_1, (1 . 13))
      --  | | | -24:(declarations_0, (1 . 6))
      --  | | | | -23:(declaration_0, (1 . 6))
      --  | | | | | 1:(IDENTIFIER, (1 . 1))   a
      --  | | | | | 2:(COLON, (3 . 3))
      --  | | | | | 3:(IDENTIFIER, (5 . 5))
      --  | | | | | 4:(SEMICOLON, (6 . 6))
      --  | | | -25:(declaration_0, (8 . 13))
      --  | | | | 5:(IDENTIFIER, (8 . 8))     b
      --  | | | | 6:(COLON, (10 . 10))
      --  | | | | 7:(IDENTIFIER, (12 . 12))
      --  | | | | 8:(SEMICOLON, (13 . 13))
      --  | | -27:(declaration_0, (15 . 20))
      --  | | | 9:(IDENTIFIER, (15 . 15))     c
      --  | | | 10:(COLON, (17 . 17))
      --  | | | 11:(IDENTIFIER, (19 . 19))
      --  | | | 12:(SEMICOLON, (20 . 20))
      --  | -29:(declaration_0, (22 . 27))
      --  | | 13:(IDENTIFIER, (22 . 22))      d
      --  | | 14:(COLON, (24 . 24))
      --  | | 15:(IDENTIFIER, (26 . 26))
      --  | | 16:(SEMICOLON, (27 . 27))
      --
      --  -31:(declaration_0, (29 . 34))
      --  | 17:(IDENTIFIER, (29 . 29))        e
      --  | 18:(COLON, (31 . 31))
      --  | 19:(IDENTIFIER, (33 . 33))
      --  | 20:(SEMICOLON, (34 . 34))),
      --
      --  21:(Wisi_EOI, (35 . 34))))

      Test_1
        ("e1",
         17,                         -- e
         Node_Expected  => (-31, 1), -- e
         Prev_Expected  => (-30, 2), -- d
         Last_Expected  => (-31, 1), -- e
         Declarations_0 => False);

      --  "e2" result stream:
      --  0:(Wisi_SOI, (1 . 1))
      --
      --  -30:(declarations_1, (1 . 27))
      --  | -28:(declarations_1, (1 . 20))
      --  | | -26:(declarations_1, (1 . 13))
      --  | | | -24:(declarations_0, (1 . 6))
      --  | | | | -23:(declaration_0, (1 . 6))
      --  | | | | | 1:(IDENTIFIER, (1 . 1))   a
      --  | | | | | 2:(COLON, (3 . 3))
      --  | | | | | 3:(IDENTIFIER, (5 . 5))
      --  | | | | | 4:(SEMICOLON, (6 . 6))
      --  | | | -25:(declaration_0, (8 . 13))
      --  | | | | 5:(IDENTIFIER, (8 . 8))     b
      --  | | | | 6:(COLON, (10 . 10))
      --  | | | | 7:(IDENTIFIER, (12 . 12))
      --  | | | | 8:(SEMICOLON, (13 . 13))
      --  | | -27:(declaration_0, (15 . 20))
      --  | | | 9:(IDENTIFIER, (15 . 15))     c
      --  | | | 10:(COLON, (17 . 17))
      --  | | | 11:(IDENTIFIER, (19 . 19))
      --  | | | 12:(SEMICOLON, (20 . 20))
      --  | -29:(declaration_0, (22 . 27))
      --  | | 13:(IDENTIFIER, (22 . 22))      d
      --  | | 14:(COLON, (24 . 24))
      --  | | 15:(IDENTIFIER, (26 . 26))
      --  | | 16:(SEMICOLON, (27 . 27))
      --
      --  17:(IDENTIFIER, (29 . 29))        e
      --
      --  18:(COLON, (31 . 31))
      --
      --  19:(IDENTIFIER, (33 . 33))
      --
      --  20:(SEMICOLON, (34 . 34)))
      --
      --  21:(Wisi_EOI, (35 . 34))))

      Test_1
        ("e2",
         18,                        -- : after e
         Node_Expected  => (18, 0), -- :
         Prev_Expected  => (17, 0), -- e
         Last_Expected  => (20, 0), -- ;
         Declarations_0 => False);
   end Breakdown_Optimized_List_01;

   procedure Breakdown_Optimized_List_02 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Syntax_Trees.AUnit_Public;

      Text : constant String := "a : A; b : B; c : C; d : D; e : E;";
      --  char_pos               |1        |10        |20        |30
      --  node_index             |1     |5     |9     |13    |18

      Tree : WisiToken.Syntax_Trees.Tree renames Optimized_List_Parser.Tree;
      procedure Test_1
        (Label   : in String;
         Index_1 : in Node_Index;
         Index_2 : in Node_Index)
      is begin
         if Trace_Tests > Detail then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line (Label & " initial parse");
         end if;

         Optimized_List_Parser.Tree.Lexer.Reset_With_String (Text);
         Optimized_List_Parser.LR_Parse (Log_File);

         Tree.Start_Edit;

         declare
            All_Terminals : constant Valid_Node_Access_Array := Tree.Get_Terminals
              (Get_Node (Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True)));

            Ref : Stream_Node_Parents := Tree.To_Stream_Node_Parents
              (Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True));
         begin
            Tree.First_Terminal (Ref, Following => False);
            for I in 1 .. Index_1 - 1 loop
               Tree.Next_Terminal (Ref);
            end loop;
            Check (Label & ".node_index_1", Tree.Get_Node_Index (Ref.Ref.Node), Index_1);

            Tree.Breakdown (Ref, Optimized_List_Parser.Productions, null, First_Terminal => True);
            Ref := Invalid_Stream_Node_Parents;

            if Trace_Tests > Detail then
               Ada.Text_IO.Put_Line (Label & " parse edited stream 1");
            end if;
            Optimized_List_Parser.LR_Parse (Log_File, Pre_Edited => True);

            if Trace_Tests > Detail then
               Ada.Text_IO.Put_Line (Label & " parsed tree 1:");
               Tree.Print_Tree;
            end if;

            Tree.Start_Edit;
            Ref := Tree.To_Stream_Node_Parents (Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True));
            Tree.First_Terminal (Ref, Following => False);
            for I in 1 .. Index_2 - 1 loop
               Tree.Next_Terminal (Ref);
            end loop;

            Check (Label & ".node_index_2", Tree.Get_Node_Index (Ref.Ref.Node), Index_2);

            Tree.Breakdown (Ref, Optimized_List_Parser.Productions, null, First_Terminal => True);

            if Trace_Tests > Detail then
               Ada.Text_IO.Put_Line (Label & " edited stream 2:");
               Tree.Print_Streams (Children => True);
               Ada.Text_IO.Put_Line (Label & "  ref: " & Tree.Image (Ref.Ref, Node_Numbers => True));
            end if;

            declare
               use all type Ada.Containers.Count_Type;
               Error_Reported : Node_Sets.Set;
            begin
               Tree.Validate_Tree
                 (User_Data, Error_Reported,
                  Node_Index_Order => False,
                  Validate_Node    => null);
               --  Mark_In_Tree doesn't work when the tree isn't fully parsed.
               if Error_Reported.Count > 0 then
                  AUnit.Assertions.Assert (False, "invalid tree");
               end if;
            end;

            --  All terminals should still be present.
            declare
               use all type SAL.Base_Peek_Type;

               Term : Terminal_Ref := Tree.First_Terminal (Tree.Stream_First (Tree.Shared_Stream, Skip_SOI => True));
               I : Positive_Index_Type := All_Terminals'First;
            begin
               if Trace_Tests > Detail then
                  Ada.Text_IO.Put ("terminals: (");
                  for T of All_Terminals loop
                     Ada.Text_IO.Put (Tree.Image (T, Node_Numbers => True) & ", ");
                  end loop;
                  Ada.Text_IO.Put_Line (")");
               end if;

               loop
                  exit when Term = Invalid_Stream_Node_Ref or else
                    Tree.ID (Term.Node) = Tree.Lexer.Descriptor.EOI_ID;

                  Check_Address (Label & ".terminals (" & Trimmed_Image (I) & ")", Term.Node, All_Terminals (I));

                  I := I + 1;
                  Tree.Next_Terminal (Term);
               end loop;
               if I /= All_Terminals'Last + 1 then
                  AUnit.Assertions.Assert (False, Label & " all_terminals length mismatch");
               end if;
            end;
         end;

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line (Label & " parse edited stream 2");
         end if;
         Optimized_List_Parser.LR_Parse (Log_File, Pre_Edited => True);

         if Trace_Tests > Detail then
            Ada.Text_IO.Put_Line (Label & " parsed tree 2:");
            Tree.Print_Tree;
         end if;
         declare
            use all type Ada.Containers.Count_Type;
            Error_Reported : Node_Sets.Set;
         begin
            Tree.Validate_Tree
              (User_Data, Error_Reported,
               Node_Index_Order => False,
               Validate_Node    => Mark_In_Tree'Access);
            if Error_Reported.Count > 0 then
               AUnit.Assertions.Assert (False, "invalid tree");
            end if;
         end;
      end Test_1;
   begin
      Test_1 ("b, c", 5, 9);
   end Breakdown_Optimized_List_02;

   procedure Prev_New_Line_01 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Syntax_Trees.AUnit_Public;

      Text : constant String :=
        "-- leading comment" & ASCII.LF &
        --        |10          |19
        "%code C %{" & ASCII.LF &
        --             |30
        "  code"  & ASCII.LF &
        --          |37
        "}% a : A; -- comment 1" & ASCII.LF & ASCII.LF &
        -- |40       |50           |60
        "b : B; -- trailing comment" & ASCII.LF;
      --         |70       |80         |88

      Parser : WisiToken.Parse.Parser.Parser'Class renames Grammar_Parser;

      procedure Test_1
        (Label                  : in String;
         Node_Pos               : in Buffer_Pos;
         Expected_Prev_Node_Pos : in Buffer_Pos;
         Expected_Index         : in SAL.Base_Peek_Type;
         Expected_Pos           : in Buffer_Pos;
         Expected_Line          : in Line_Number_Type)
      is
         use SAL.AUnit;
         use WisiToken.AUnit;

         This_Node          : constant Valid_Node_Access := Parser.Tree.Find_Byte_Pos (Node_Pos, True);
         Expected_Prev_Node : constant Valid_Node_Access := Parser.Tree.Find_Byte_Pos
           (Expected_Prev_Node_Pos, True);
         Computed_Prev      : constant New_Line_Ref      := Parser.Tree.Prev_New_Line (This_Node);
      begin
         if Trace_Tests > Extra then
            Parser.Tree.Lexer.Trace.Put_Line (Label & ".this_node: " & Parser.Tree.Image (This_Node));
            Parser.Tree.Lexer.Trace.Put_Line (Label & ".computed_prev_node: " & Parser.Tree.Image (Computed_Prev.Node));
            Parser.Tree.Lexer.Trace.Put_Line (Label & ".expected_prev_node: " & Parser.Tree.Image (Expected_Prev_Node));
         end if;
         Check_Address (Label & ".node", Computed_Prev.Node, Expected_Prev_Node);
         Check (Label & ".index", Computed_Prev.Non_Grammar_Index, Expected_Index);
         Check (Label & ".pos", Computed_Prev.Pos, Expected_Pos);
         Check (Label & ".line", Computed_Prev.Line, Expected_Line);
      end Test_1;

   begin
      Parser.Tree.Lexer.Reset_With_String (Text);

      Parser.LR_Parse (Log_File);

      if Trace_Tests > Detail then
         Parser.Tree.Print_Tree (Non_Grammar => True);
         Parser.Put_Errors;
      end if;

      Test_1 ("1a", 1, 1, 1, 1, 1);
      Test_1 ("2", 20, 1, 2, 19, 2);
      Test_1 ("4", 40, 28, 0, 37, 4);
      Test_1 ("6", 62, 46, 2, 61, 6);
      Test_1 ("7", 89, 67, 1, 88, 7);
   end Prev_New_Line_01;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Left_Breakdown_1'Access, "Left_Breakdown_1");
      Register_Routine (T, Find_New_Line_1'Access, "Find_New_Line_1");
      Register_Routine (T, Find_New_Line_2'Access, "Find_New_Line_2");
      Register_Routine (T, Byte_Region_1'Access, "Byte_Region_1");
      Register_Routine (T, Line_At_Byte_Pos_1'Access, "Line_At_Byte_Pos_1");
      Register_Routine (T, Line_At_Byte_Pos_2'Access, "Line_At_Byte_Pos_2");
      Register_Routine (T, Line_At_Byte_Pos_3'Access, "Line_At_Byte_Pos_3");
      Register_Routine (T, Find_Char_Pos_1'Access, "Find_Char_Pos_1");
      Register_Routine (T, Breakdown_Optimized_List_01'Access, "Breakdown_Optimized_List_01");
      Register_Routine (T, Breakdown_Optimized_List_02'Access, "Breakdown_Optimized_List_02");
      Register_Routine (T, Prev_New_Line_01'Access, "Prev_New_Line_01");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'("test_syntax_trees.adb");
   end Name;

end Test_Syntax_Trees;
--  Local Variables:
--  ada-case-strict: nil
--  End:
