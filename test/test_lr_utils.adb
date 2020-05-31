--  Abstract:
--
--  See spec
--
--  Copyright (C) 2018 - 2020 Stephen Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Checks.Containers;
with Ada.Containers;
with Ada.Text_IO;
with SAL.Gen_Bounded_Definite_Stacks;
with WisiToken.AUnit;
with WisiToken.Syntax_Trees.LR_Utils; use WisiToken.Syntax_Trees.LR_Utils;
package body Test_LR_Utils is
   use WisiToken;
   use WisiToken.Syntax_Trees;

   procedure Check
     (Label    : in String;
      Computed : in Constant_List'Class;
      Expected : in Valid_Node_Index_Array)
   is
      use all type SAL.Base_Peek_Type;
      use AUnit.Checks.Containers;
      use WisiToken.AUnit;
      Iter : constant Constant_Iterator := Computed.Iterate_Constant;
      Cur  : Cursor            := Computed.First;
   begin
      Check (Label & ".count", Computed.Count, Ada.Containers.Count_Type (Expected'Length));

      for I in Expected'Range loop
         Check (Label & "." & I'Image, Get_Node (Cur), Expected (I));
         if Computed in List then
            declare
               Computed_List : List renames List (Computed);
            begin
               if Computed_List.Separator_ID /= Invalid_Token_ID and I > Expected'First then
                  Check
                    (Label & ".separator",
                     Computed.Tree.ID (Computed.Tree.Child (Computed.Tree.Parent (Get_Node (Cur)), 2)),
                     Computed_List.Separator_ID);
               end if;
            end;
         end if;
         Cur := Iter.Next (Cur);
      end loop;
   end Check;

   package Stacks is new SAL.Gen_Bounded_Definite_Stacks (Node_Index);

   generic
      type Token_Enum_ID is (<>);
      Tree        : in out Syntax_Trees.Tree;
      Stack       : in out Stacks.Stack;
      with function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID;
   procedure Gen_Reduce
     (Token_Count : in SAL.Base_Peek_Type;
      LHS         : in Token_Enum_ID;
      RHS         : in Natural);

   procedure Gen_Reduce
     (Token_Count : in SAL.Base_Peek_Type;
      LHS         : in Token_Enum_ID;
      RHS         : in Natural)
   is
      Children : Valid_Node_Index_Array (1 .. Token_Count);
   begin
      for I in reverse Children'Range loop
         Children (I) := Stack.Pop;
      end loop;
      Stack.Push (Tree.Add_Nonterm ((+LHS, RHS), Children));
   end Gen_Reduce;

   ----------
   --  Test subprograms

   procedure Test_Insert_1 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      type Token_Enum_ID is
        (SEPARATOR_ID,
         IDENTIFIER_ID,
         List_ID,
         Element_ID);

      function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
      is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

      Descriptor : aliased constant WisiToken.Descriptor :=
        (First_Terminal       => +SEPARATOR_ID,
         Last_Terminal        => +IDENTIFIER_ID,
         First_Nonterminal    => +List_ID,
         Last_Nonterminal     => +Element_ID,
         EOI_ID               => Invalid_Token_ID,
         Accept_ID            => Invalid_Token_ID,
         Case_Insensitive     => False,
         New_Line_ID          => Invalid_Token_ID,
         String_1_ID          => Invalid_Token_ID,
         String_2_ID          => Invalid_Token_ID,
         Image                =>
           (new String'("SEPARATOR"),
            new String'("IDENTIFIER"),
            new String'("List"),
            new String'("Element")),
         Terminal_Image_Width => 13,
         Image_Width          => 21,
         Last_Lookahead       => Invalid_Token_ID);

      Terminals   : aliased Base_Token_Arrays.Vector;
      Shared_Tree : aliased WisiToken.Syntax_Trees.Base_Tree;
      Tree        : WisiToken.Syntax_Trees.Tree;
      Stack       : Stacks.Stack (10);

      procedure Reduce is new Gen_Reduce (Token_Enum_ID, Tree, Stack, "+");

   begin
      --  Create a tree duplicating example in LR_Utils.Insert:
      --
      --  15: list: Tree.Root
      --  13: | list
      --  11: | | list = Parent
      --  09: | | | list
      --  08: | | | | element: First
      --  02: | | | separator
      --  10: | | | element: After
      --  04: | | separator
      --  12: | | element: Before
      --  06: | separator
      --  14: | element: Last

      Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => True, Set_Parents => True);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (1, 1), others => <>));
      Terminals (1).Tree_Index := Tree.Add_Terminal (1, Terminals);

      Terminals.Append ((+SEPARATOR_ID, Invalid_Node_Index, (2, 2), others => <>));
      Terminals (2).Tree_Index := Tree.Add_Terminal (2, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (3, 3), others => <>));
      Terminals (3).Tree_Index := Tree.Add_Terminal (3, Terminals);

      Terminals.Append ((+SEPARATOR_ID, Invalid_Node_Index, (4, 4), others => <>));
      Terminals (4).Tree_Index := Tree.Add_Terminal (4, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (5, 5), others => <>));
      Terminals (5).Tree_Index := Tree.Add_Terminal (5, Terminals);

      Terminals.Append ((+SEPARATOR_ID, Invalid_Node_Index, (6, 6), others => <>));
      Terminals (6).Tree_Index := Tree.Add_Terminal (6, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (7, 7), others => <>));
      Terminals (7).Tree_Index := Tree.Add_Terminal (7, Terminals);

      Stack.Push (1);
      Reduce (1, Element_ID, 0); -- node 8
      Reduce (1, List_ID, 0);

      Stack.Push (2);
      Stack.Push (3);
      Reduce (1, Element_ID, 0); -- 10
      Reduce (3, List_ID, 1);

      Stack.Push (4);
      Stack.Push (5);
      Reduce (1, Element_ID, 0); -- 12
      Reduce (3, List_ID, 1);

      Stack.Push (6);
      Stack.Push (7);
      Reduce (1, Element_ID, 0); -- 14
      Reduce (3, List_ID, 1);

      Tree.Set_Root (Stack.Pop);

      if WisiToken.Trace_Generate_Table > WisiToken.Outline then
         Ada.Text_IO.Put_Line ("before insert");
         Tree.Print_Tree (Descriptor);
      end if;

      declare
         use WisiToken.AUnit;

         List : LR_Utils.List := Creators.Create_List (Tree, Tree.Root, +List_ID, +Element_ID, +SEPARATOR_ID);

         Pre_Expected : constant Valid_Node_Index_Array := (8, 10, 12, 14);

         --  After Insert:
         --
         --  15: list: Tree.Root
         --  13: | list
         --  11: | | list = Parent
         --  17: | | | list: new_list_nonterm
         --  09: | | | | list
         --  08: | | | | | element: First
         --  02: | | | | separator
         --  10: | | | | element: After
         --  18: | | | separator
         --  17: | | | element: new
         --  04: | | separator
         --  12: | | element: Before
         --  06: | separator
         --  14: | element: Last

         Post_Expected : constant Valid_Node_Index_Array := (8, 10, 17, 12, 14);

         New_Element : constant Valid_Node_Index := Tree.Add_Nonterm -- 17
           ((+Element_ID, 0),
            (1 => Tree.Add_Terminal (+IDENTIFIER_ID))); -- 16
      begin
         Check ("root", Tree.Root, 15);

         Check ("pre", List, Pre_Expected);

         Insert (List, New_Element, After => List.To_Cursor (10)); -- creates separator 18

         if WisiToken.Trace_Generate_Table > Outline then
            Ada.Text_IO.Put_Line ("after insert");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("post", List, Post_Expected);
      end;
   end Test_Insert_1;

   procedure Test_Delete_1 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      type Token_Enum_ID is
        (SEPARATOR_ID,
         IDENTIFIER_ID,
         List_ID,
         Element_ID,
         Other_ID);

      function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
      is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

      Descriptor : aliased constant WisiToken.Descriptor :=
        (First_Terminal       => +SEPARATOR_ID,
         Last_Terminal        => +IDENTIFIER_ID,
         First_Nonterminal    => +List_ID,
         Last_Nonterminal     => +Other_ID,
         EOI_ID               => Invalid_Token_ID,
         Accept_ID            => Invalid_Token_ID,
         Case_Insensitive     => False,
         New_Line_ID          => Invalid_Token_ID,
         String_1_ID          => Invalid_Token_ID,
         String_2_ID          => Invalid_Token_ID,
         Image                =>
           (new String'("SEPARATOR"),
            new String'("IDENTIFIER"),
            new String'("List"),
            new String'("Element"),
            new String'("Other")),
         Terminal_Image_Width => 13,
         Image_Width          => 21,
         Last_Lookahead       => Invalid_Token_ID);

      Terminals   : aliased Base_Token_Arrays.Vector;
      Shared_Tree : aliased WisiToken.Syntax_Trees.Base_Tree;
      Tree        : WisiToken.Syntax_Trees.Tree;
      Stack       : Stacks.Stack (10);

      procedure Reduce is new Gen_Reduce (Token_Enum_ID, Tree, Stack, "+");

   begin
      --  Create a tree duplicating example in LR_Utils.Insert:
      --
      --  15: list: Tree.Root
      --  13: | list
      --  11: | | list
      --  09: | | | list
      --  08: | | | | element: 1, First
      --  02: | | | separator
      --  10: | | | element: 2
      --  04: | | separator
      --  12: | | element: 3
      --  06: | separator
      --  14: | element: 4, Last

      Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => True, Set_Parents => True);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (1, 1), others => <>));
      Terminals (1).Tree_Index := Tree.Add_Terminal (1, Terminals);

      Terminals.Append ((+SEPARATOR_ID, Invalid_Node_Index, (2, 2), others => <>));
      Terminals (2).Tree_Index := Tree.Add_Terminal (2, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (3, 3), others => <>));
      Terminals (3).Tree_Index := Tree.Add_Terminal (3, Terminals);

      Terminals.Append ((+SEPARATOR_ID, Invalid_Node_Index, (4, 4), others => <>));
      Terminals (4).Tree_Index := Tree.Add_Terminal (4, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (5, 5), others => <>));
      Terminals (5).Tree_Index := Tree.Add_Terminal (5, Terminals);

      Terminals.Append ((+SEPARATOR_ID, Invalid_Node_Index, (6, 6), others => <>));
      Terminals (6).Tree_Index := Tree.Add_Terminal (6, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (7, 7), others => <>));
      Terminals (7).Tree_Index := Tree.Add_Terminal (7, Terminals);

      Stack.Push (1);
      Reduce (1, Element_ID, 0); -- node 8
      Reduce (1, List_ID, 0);

      Stack.Push (2);
      Stack.Push (3);
      Reduce (1, Element_ID, 0); -- 10
      Reduce (3, List_ID, 1);

      Stack.Push (4);
      Stack.Push (5);
      Reduce (1, Element_ID, 0); -- 12
      Reduce (3, List_ID, 1);

      Stack.Push (6);
      Stack.Push (7);
      Reduce (1, Element_ID, 0); -- 14
      Reduce (3, List_ID, 1);

      Tree.Set_Root (Stack.Pop);

      if WisiToken.Trace_Generate_Table > WisiToken.Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("1 before");
         Tree.Print_Tree (Descriptor);
      end if;

      declare
         use WisiToken.AUnit;
         use AUnit.Checks;

         List : LR_Utils.List := Creators.Create_List (Tree, Tree.Root, +List_ID, +Element_ID, +SEPARATOR_ID);

         --  See body of LR_Utils for comments with After list structure.

         Cur : Cursor := List.To_Cursor (12);
      begin
         Check ("0 tree root", Tree.Root, 15);
         Check ("0 list", List, (8, 10, 12, 14));
         Check ("0 list root parent", Tree.Parent (List.Root), Invalid_Node_Index);

         Delete (List, Cur);

         if WisiToken.Trace_Generate_Table > Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("1 after");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("1 list", List, (8, 10, 14));
         Check ("1 list root", List.Root, 15);
         Check ("1 list root parent", Tree.Parent (List.Root), Invalid_Node_Index);
         Check ("1 cur", Has_Element (Cur), False);

         Cur := List.First;
         List.Delete (Cur);

         if WisiToken.Trace_Generate_Table > Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("2 after");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("2 list", List, (10, 14));
         Check ("2 list root", List.Root, 15);
         Check ("2 list root parent", Tree.Parent (List.Root), Invalid_Node_Index);

         Cur := List.Last;
         List.Delete (Cur);

         if WisiToken.Trace_Generate_Table > Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("3 after");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("3 list", List, (1 => 10));
         Check ("3 list root", List.Root, 11);
         Check ("3 list root parent", Tree.Parent (List.Root), Invalid_Node_Index);
         Check ("3 tree root", Tree.Root, 11);

         Cur := List.First;
         List.Delete (Cur);

         if WisiToken.Trace_Generate_Table > Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("4 after");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("4 list", List, (1 .. 0 => 0));
         Check ("4 list root", List.Root, Invalid_Node_Index);
         Check ("4 tree root", Tree.Root, Invalid_Node_Index);
      end;

      --  test delete last with node parent.
      declare
         use WisiToken.AUnit;

         List : LR_Utils.List := Creators.Empty_List
           (Tree,
            List_ID           => +List_ID,
            Multi_Element_RHS => 1,
            Element_ID        => +Element_ID,
            Separator_ID      => Invalid_Token_ID);

         Cur : Cursor;
      begin
         List.Append (Tree.Add_Nonterm ((+Element_ID, 1), (1 .. 0 => 1)));
         List.Append (Tree.Add_Nonterm ((+Element_ID, 2), (1 .. 0 => 1)));
         Tree.Set_Root (Tree.Add_Nonterm ((+Other_ID, 0), (1 => List.Root)));
         Check ("5 list root", List.Root, 19);
         Check ("5 tree root", Tree.Root, 20);

         Cur := List.Last;

         List.Delete (Cur);
         Check ("6 list root", List.Root, 17);
         Check ("6 tree root", Tree.Root, 20);
      end;
   end Test_Delete_1;

   procedure Test_Copy_Skip (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;

      type Token_Enum_ID is
        (SEPARATOR_1_ID,
         IDENTIFIER_ID,
         LEFT_BRACKET_ID,
         RIGHT_BRACKET_ID,
         List_1_ID,
         Element_1_ID,
         List_2_ID,
         Other_1_ID,
         Other_2_ID,
         Top_ID);

      function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
      is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

      Descriptor : aliased constant WisiToken.Descriptor :=
        (First_Terminal       => +SEPARATOR_1_ID,
         Last_Terminal        => +RIGHT_BRACKET_ID,
         First_Nonterminal    => +List_1_ID,
         Last_Nonterminal     => +Top_ID,
         EOI_ID               => Invalid_Token_ID,
         Accept_ID            => Invalid_Token_ID,
         Case_Insensitive     => False,
         New_Line_ID          => Invalid_Token_ID,
         String_1_ID          => Invalid_Token_ID,
         String_2_ID          => Invalid_Token_ID,
         Image                =>
           (new String'("SEPARATOR_1"),
            new String'("IDENTIFIER"),
            new String'("LEFT_BRACKET"),
            new String'("RIGHT_BRACKET"),
            new String'("List_1"),
            new String'("Element_1"),
            new String'("List_2"),
            new String'("Other_1"),
            new String'("Other_2"),
            new String'("Top")),
         Terminal_Image_Width => 13,
         Image_Width          => 21,
         Last_Lookahead       => Invalid_Token_ID);

      Terminals   : aliased Base_Token_Arrays.Vector;
      Shared_Tree : aliased WisiToken.Syntax_Trees.Base_Tree;
      Tree        : WisiToken.Syntax_Trees.Tree;
      Stack       : Stacks.Stack (10);

      procedure Reduce is new Gen_Reduce (Token_Enum_ID, Tree, Stack, "+");

      List_Root    : Valid_Node_Index_Array (1 .. 3);
      Skip_Element : Valid_Node_Index_Array (1 .. 3);
   begin
      --  Create a tree containing a nested list, copy it excluding one item in the nested list.
      --
      --  25: list_1
      --  23: | list_1
      --  21: | | list_1
      --  12: | | | list_1
      --  11: | | | | element_1
      --  01: | | | | | 1:identifier
      --  02: | | | 2:separator_1
      --  20: | | | element_1
      --  19: | | | | other_1
      --  18: | | | | | other_2
      --  03: | | | | | | 3:left_bracket
      --  17: | | | | | | list_2
      --  16: | | | | | | | list_1
      --  14: | | | | | | | | list_1
      --  13: | | | | | | | | | element_1
      --  04: | | | | | | | | | | 4:identifier
      --  15: | | | | | | | | element_1
      --  05: | | | | | | | | | 5:identifier -- skip this
      --  06: | | | | | | 6:right_bracket
      --  07: | | 7:separator_1
      --  22: | | element_1
      --  08: | | | 8:identifier
      --  09: | 9:separator_1
      --  24: | element_1
      --  10: | | 10:identifier

      Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => True, Set_Parents => True);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (1, 1), others => <>));
      Terminals (1).Tree_Index := Tree.Add_Terminal (1, Terminals);

      Terminals.Append ((+SEPARATOR_1_ID, Invalid_Node_Index, (2, 2), others => <>));
      Terminals (2).Tree_Index := Tree.Add_Terminal (2, Terminals);

      Terminals.Append ((+LEFT_BRACKET_ID, Invalid_Node_Index, (3, 3), others => <>));
      Terminals (3).Tree_Index := Tree.Add_Terminal (3, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (4, 4), others => <>));
      Terminals (4).Tree_Index := Tree.Add_Terminal (4, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (5, 5), others => <>));
      Terminals (5).Tree_Index := Tree.Add_Terminal (5, Terminals);

      Terminals.Append ((+RIGHT_BRACKET_ID, Invalid_Node_Index, (6, 6), others => <>));
      Terminals (6).Tree_Index := Tree.Add_Terminal (6, Terminals);

      Terminals.Append ((+SEPARATOR_1_ID, Invalid_Node_Index, (7, 7), others => <>));
      Terminals (7).Tree_Index := Tree.Add_Terminal (7, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (8, 8), others => <>));
      Terminals (8).Tree_Index := Tree.Add_Terminal (8, Terminals);

      Terminals.Append ((+SEPARATOR_1_ID, Invalid_Node_Index, (9, 9), others => <>));
      Terminals (9).Tree_Index := Tree.Add_Terminal (9, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (10, 10), others => <>));
      Terminals (10).Tree_Index := Tree.Add_Terminal (10, Terminals);

      Stack.Push (1);
      Reduce (1, Element_1_ID, 0);
      Reduce (1, List_1_ID, 0);

      Stack.Push (2);
      Stack.Push (3);

      Stack.Push (4);
      Reduce (1, Element_1_ID, 0);
      Reduce (1, List_1_ID, 0);

      Stack.Push (5);
      Reduce (1, Element_1_ID, 0);
      Skip_Element (3) := Stack.Peek;
      Reduce (2, List_1_ID, 1);
      List_Root (3)    := Stack.Peek;
      Skip_Element (2) := Stack.Peek;
      Reduce (1, List_2_ID, 0);
      List_Root (2) := Stack.Peek;

      Stack.Push (6);
      Reduce (3, Other_2_ID, 0);
      Reduce (1, Other_1_ID, 0);
      Reduce (1, Element_1_ID, 2);
      Skip_Element (1) := Stack.Peek;
      Reduce (3, List_1_ID, 1);

      Stack.Push (7);
      Stack.Push (8);
      Reduce (1, Element_1_ID, 0);
      Reduce (3, List_1_ID, 1);

      Stack.Push (9);
      Stack.Push (10);
      Reduce (1, Element_1_ID, 0);
      Reduce (3, List_1_ID, 1);
      List_Root (1) := Stack.Peek;

      Reduce (1, Top_ID, 0);

      Tree.Set_Root (Stack.Pop);

      --  First copy just the inner list, to test a simpler case.
      declare
         Inner_List : constant Constant_List := Creators.Create_List
           (Tree, List_Root (3), +List_1_ID, +Element_1_ID);

         Skip_List : constant Skip_Info :=
           (Skip_Last               => 1,
            Start_List_Root         => Inner_List.Root,
            Start_List_ID           => +List_1_ID,
            Start_Element_ID        => +Element_1_ID,
            Start_Separator_ID      => Invalid_Token_ID,
            Start_Multi_Element_RHS => 1,
            Skips                   => (1 => (Skip, Skip_Element (3))));

         Pre_Expected  : constant Valid_Node_Index_Array := (13, 15);
         Post_Expected : constant Valid_Node_Index_Array := (1 => 28);

         Inner_List_Copy : Node_Index;
      begin
         if WisiToken.Trace_Generate_Table > WisiToken.Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("pre copy_skip inner");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("1 inner.pre", Inner_List, Pre_Expected);

         Inner_List_Copy := Copy_Skip_Nested (Skip_List, Tree);

         Stack.Push (Inner_List_Copy);
         Stack.Push (Tree.Root);

         Reduce (2, Top_ID, 1);

         Tree.Set_Root (Stack.Pop);

         if WisiToken.Trace_Generate_Table > Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("post copy_skip inner");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("1 inner.post source", Inner_List, Pre_Expected);
         Check ("1 inner.post dest", Creators.Create_List (Inner_List, Tree, Inner_List_Copy), Post_Expected);
      end;

      --  Now copy the outer list, skipping the same item
      declare
         Outer_List : constant Constant_List := Creators.Create_List
           (Tree, List_Root (1), +List_1_ID, +Element_1_ID);
         Inner_List : constant Constant_List := Creators.Create_List
           (Tree, List_Root (3), +List_1_ID, +Element_1_ID);

         Skip_List : constant Skip_Info :=
           (Skip_Last               => 3,
            Start_List_Root         => Outer_List.Root,
            Start_List_ID           => Outer_List.List_ID,
            Start_Element_ID        => Outer_List.Element_ID,
            Start_Separator_ID      => +SEPARATOR_1_ID,
            Start_Multi_Element_RHS => 1,
            Skips                   =>
              (1                    =>
                 (Nested,
                  Element           => Skip_Element (1),
                  List_Root         => List_Root (2),
                  List_ID           => +List_2_ID,
                  Element_ID        => +List_1_ID,
                  Multi_Element_RHS => 1,
                  Separator_ID      => Invalid_Token_ID),
               2                    =>
                 (Nested,
                  Element           => Skip_Element (2),
                  List_Root         => List_Root (3),
                  List_ID           => +List_1_ID,
                  Element_ID        => +Element_1_ID,
                  Multi_Element_RHS => 1,
                  Separator_ID      => Invalid_Token_ID),
               3                    =>
               (Skip, Skip_Element (3))));

         Pre_Outer_Expected  : constant Valid_Node_Index_Array := (11, 20, 22, 24);
         Pre_Inner_Expected  : constant Valid_Node_Index_Array := (13, 15);

         Post_Outer_Expected : constant Valid_Node_Index_Array := (32, 42, 46, 50);
         Post_Inner_Expected : constant Valid_Node_Index_Array := (1 => 36);

         Outer_List_Copy : Node_Index;
      begin
         Check ("2 inner.pre", Inner_List, Pre_Inner_Expected);
         Check ("2 outer.pre", Outer_List, Pre_Outer_Expected);

         Outer_List_Copy := Copy_Skip_Nested (Skip_List, Tree);

         Stack.Push (Outer_List_Copy);
         Stack.Push (Tree.Root);

         Reduce (2, Top_ID, 1);

         Tree.Set_Root (Stack.Pop);

         if WisiToken.Trace_Generate_Table > Outline then
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("post copy_skip outer");
            Tree.Print_Tree (Descriptor);
         end if;

         Check ("outer.post source", Outer_List, Pre_Outer_Expected);
         Check ("inner.post source", Inner_List, Pre_Inner_Expected);

         Check ("outer.post dest", Creators.Create_List (Outer_List, Tree, Outer_List_Copy), Post_Outer_Expected);
         Check ("inner.post dest", Creators.Create_List (Inner_List, Tree, 37), Post_Inner_Expected);

         --  Check that "other" nodes got copied correctly.
         declare
            use WisiToken.AUnit;
            use AUnit.Checks.Containers;
            Node : Node_Index := Tree.Parent (Post_Inner_Expected (1));
         begin
            Check ("other 0.ID", Tree.ID (Node), +List_1_ID);

            Node := Tree.Parent (Node, 2);
            Check ("other 1.ID", Tree.ID (Node), +Other_2_ID);
            Check ("other 1.child_count", Tree.Child_Count (Node), 3);
            Check ("other 1.1", Tree.ID (Tree.Child (Node, 1)), +LEFT_BRACKET_ID);
            Check ("other 1.3", Tree.ID (Tree.Child (Node, 3)), +RIGHT_BRACKET_ID);

            Node := Tree.Parent (Node);
            Check ("other 2.ID", Tree.ID (Node), +Other_1_ID);

            Node := Tree.Parent (Node);
            Check ("other 3", Node, 42);
         end;
      end;
   end Test_Copy_Skip;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Insert_1'Access, "Test_Insert_1");
      Register_Routine (T, Test_Delete_1'Access, "Test_Delete_1");
      Register_Routine (T, Test_Copy_Skip'Access, "Test_Copy_Skip");
   end Register_Tests;

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_lr_utils.adb");
   end Name;

end Test_LR_Utils;
--  Local Variables:
--  ada-case-strict: nil
--  End:
