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

with WisiToken.Syntax_Trees.AUnit_Public;
with WisiToken.Syntax_Trees.AUnit_Private;
package body WisiToken.Syntax_Trees.Test is

   --  Example tokens taken from ada_lite.wy. We don't use Ada_Lite
   --  directly, to reduce dependencies for this test, and simplify things.

   type Token_Enum_ID is
     (END_ID,
      IDENTIFIER_ID,
      IS_ID,
      LEFT_PAREN_ID,
      PACKAGE_ID,
      PROCEDURE_ID,
      RIGHT_PAREN_ID,
      SEMICOLON_ID,
      Wisi_EOI_ID,
      aspect_specification_opt_ID,
      declaration_ID,
      formal_part_ID,
      name_opt_ID,
      param_list_ID,
      procedure_specification_ID,
      package_declaration_ID,
      package_specification_ID
     );

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

   ----------
   --  Test subprograms

   procedure Test_Move_Branch_Point (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Syntax_Trees.AUnit_Private;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use all type Node_Arrays.Vector;
      use all type Valid_Node_Index_Arrays.Vector;

      Terminals     : aliased Base_Token_Arrays.Vector;
      Shared_Tree   : aliased WisiToken.Syntax_Trees.Base_Tree;
      Branched_Tree : WisiToken.Syntax_Trees.Tree;
      Junk          : Node_Index;
      pragma Unreferenced (Junk);
      Node_Ident_1  : Node_Index;
      Node_Ident_2  : Node_Index;
      Node_Name     : Node_Index;

      Expected_Branched_Nodes : Node_Arrays.Vector;
   begin
      --  Create a branched tree, set a child of a new node to a shared
      --  node, thus invoking Move_Branch_Point.

      Branched_Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => True);

      Terminals.Append ((+PROCEDURE_ID, Invalid_Node_Index, (1, 9), others => <>));
      Junk := Branched_Tree.Add_Terminal (Terminals.Last_Index, Terminals); -- 1

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (11, 16), others => <>));
      Node_Ident_1 := Branched_Tree.Add_Terminal (Terminals.Last_Index, Terminals); -- 2

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (18, 19), others => <>));
      Junk := Branched_Tree.Add_Terminal (Terminals.Last_Index, Terminals); -- 3

      Branched_Tree.Set_Flush_False;

      Node_Name := Branched_Tree.Add_Nonterm
        ((+name_opt_ID, 0), Children => (1 => Node_Ident_1), Action => null, Default_Virtual => False); -- 4
      --  moves branch point; Branched_Tree.Last_Shared_Node is now 1.

      Check ("node 4", Node_Name, 4);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (21, 22), others => <>));
      Node_Ident_2 := Branched_Tree.Add_Terminal (Terminals.Last_Index, Terminals); -- 5

      Check ("node 5", Node_Ident_2, 5);

      Expected_Branched_Nodes.Set_First_Last (2, 1);
      Expected_Branched_Nodes.Append
        ((Shared_Terminal,
          Parent      => 4,
          Terminal    => 2,
          ID          => +IDENTIFIER_ID,
          Byte_Region => (11, 16),
          others      => <>)); -- 2

      Expected_Branched_Nodes.Append
        ((Shared_Terminal,
          Parent      => 0,
          Terminal    => 3,
          ID          => +IDENTIFIER_ID,
          Byte_Region =>  (18, 19),
          others      => <>)); -- 3

      Expected_Branched_Nodes.Append
        ((Nonterm,
          Parent   => 0,
          ID       => +name_opt_ID,
          Children => +2,
          others   => <>)); -- 4

      Expected_Branched_Nodes.Append
        ((Shared_Terminal,
          Parent      => 0,
          ID          => +IDENTIFIER_ID,
          Byte_Region =>  (21, 22),
          Terminal    => 4,
          others      => <>)); -- 5

      Check
        ("branched tree",
         Branched_Tree,
         ((Ada.Finalization.Controlled with
           Shared_Tree      => Shared_Tree'Unchecked_Access,
           Last_Shared_Node => 1,
           Branched_Nodes   => Expected_Branched_Nodes,
           Flush            => False,
           Root             => Invalid_Node_Index)));

   end Test_Move_Branch_Point;

   procedure Test_Prev_Next_Terminal_1 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Syntax_Trees.AUnit_Public;

      Terminals   : aliased Base_Token_Arrays.Vector;
      Shared_Tree : aliased WisiToken.Syntax_Trees.Base_Tree;
      Tree        : WisiToken.Syntax_Trees.Tree;
      Param_List  : Node_Index;
      Formal_Part : Node_Index;
      Name        : Node_Index;
      pragma Unreferenced (Name);
   begin
      --  Create a tree representing the parse of:
      --
      --  procedure Proc_1 (  )
      --  1         11     17 20
      --  1         2      3  4
      --
      --  Note that param_list is empty

      Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => True);

      Terminals.Append ((+PROCEDURE_ID, Invalid_Node_Index, (1, 9), others => <>));
      Terminals (1).Tree_Index := Tree.Add_Terminal (1, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (11, 16), others => <>));
      Terminals (2).Tree_Index := Tree.Add_Terminal (2, Terminals);

      Terminals.Append ((+LEFT_PAREN_ID, Invalid_Node_Index, (17, 17), others => <>));
      Terminals (3).Tree_Index := Tree.Add_Terminal (3, Terminals);

      Terminals.Append ((+RIGHT_PAREN_ID, Invalid_Node_Index, (26, 26), others => <>));
      Terminals (4).Tree_Index := Tree.Add_Terminal (4, Terminals);

      Param_List := Tree.Add_Nonterm -- 5
        ((+param_list_ID, 0),
         (1 .. 0 => Invalid_Node_Index));

      Formal_Part := Tree.Add_Nonterm -- 6
        ((+formal_part_ID, 0),
         (1 => Terminals (3).Tree_Index,
          2 => Param_List,
          3 => Terminals (4).Tree_Index));

      Name := Tree.Add_Nonterm -- 7
        ((+name_opt_ID, 0),
         (1 => Terminals (1).Tree_Index,
          2 => Terminals (2).Tree_Index,
          3 => Formal_Part));

      Check ("prev right_paren", Tree.Prev_Terminal (4), 3);
      Check ("prev procedure", Tree.Prev_Terminal (1), Invalid_Node_Index);
      Check ("prev left_paren", Tree.Prev_Terminal (3), 2);

      Check ("next right_paren", Tree.Next_Terminal (4), Invalid_Node_Index);
      Check ("next procedure", Tree.Next_Terminal (1), 2);
      Check ("next name", Tree.Next_Terminal (2), 3);
      Check ("next left_paren", Tree.Next_Terminal (3), 4);

      --  Add more tokens, representing:
      --
      --  package Pack_1 is
      --    procedure Proc_1 ();
      --  end;
      --
      --  note the trailing name is empty
   end Test_Prev_Next_Terminal_1;

   procedure Test_Prev_Next_Terminal_2 (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.Syntax_Trees.AUnit_Public;

      Terminals                : aliased Base_Token_Arrays.Vector;
      Shared_Tree              : aliased WisiToken.Syntax_Trees.Base_Tree;
      Tree                     : WisiToken.Syntax_Trees.Tree;

      Virtual_End              : Node_Index;
      Virtual_Semicolon        : Node_Index;
      Param_List               : Node_Index;
      Formal_Part              : Node_Index;
      Procedure_Specification  : Node_Index;
      Declaration              : Node_Index;
      Aspect_Specification_Opt : Node_Index;
      Name_Opt                 : Node_Index;
      Package_Specification    : Node_Index;
      Package_Declaration      : Node_Index;
   begin
      --  Create a tree representing the parse of:
      --
      --  package Pack_1 is procedure Proc_1 (  )  ;  end; EOI
      --  1       9      16 19        29     36 39 42
      --  1       2      3  4         5      6  7  8       9
      --
      --  'end;' is inserted by error recover; the trailing name is empty

      Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => True);

      Terminals.Append ((+PACKAGE_ID, Invalid_Node_Index, (1, 7), others => <>));
      Terminals (1).Tree_Index := Tree.Add_Terminal (1, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (9, 14), others => <>));
      Terminals (2).Tree_Index := Tree.Add_Terminal (2, Terminals);

      Terminals.Append ((+IS_ID, Invalid_Node_Index, (16, 17), others => <>));
      Terminals (3).Tree_Index := Tree.Add_Terminal (3, Terminals);

      Terminals.Append ((+PROCEDURE_ID, Invalid_Node_Index, (19, 27), others => <>));
      Terminals (4).Tree_Index := Tree.Add_Terminal (4, Terminals);

      Terminals.Append ((+IDENTIFIER_ID, Invalid_Node_Index, (29, 34), others => <>));
      Terminals (5).Tree_Index := Tree.Add_Terminal (5, Terminals);

      Terminals.Append ((+LEFT_PAREN_ID, Invalid_Node_Index, (36, 36), others => <>));
      Terminals (6).Tree_Index := Tree.Add_Terminal (6, Terminals);

      Terminals.Append ((+RIGHT_PAREN_ID, Invalid_Node_Index, (39, 39), others => <>));
      Terminals (7).Tree_Index := Tree.Add_Terminal (7, Terminals);

      Terminals.Append ((+SEMICOLON_ID, Invalid_Node_Index, (42, 42), others => <>));
      Terminals (8).Tree_Index := Tree.Add_Terminal (8, Terminals);

      Terminals.Append ((+Wisi_EOI_ID, Invalid_Node_Index, (43, 42), others => <>));
      Terminals (9).Tree_Index := Tree.Add_Terminal (9, Terminals);

      Virtual_End := Tree.Add_Terminal (+END_ID, Before => 9); -- 10

      Virtual_Semicolon := Tree.Add_Terminal (+SEMICOLON_ID, Before => 9); -- 11

      Param_List := Tree.Add_Nonterm -- 12
        ((+param_list_ID, 0),
         (1 .. 0 => Invalid_Node_Index));

      Formal_Part := Tree.Add_Nonterm -- 13
        ((+formal_part_ID, 0),
         (1 => Terminals (6).Tree_Index,
          2 => Param_List,
          3 => Terminals (7).Tree_Index));

      Procedure_Specification := Tree.Add_Nonterm -- 14
        ((+procedure_specification_ID, 0),
         (1 => Terminals (4).Tree_Index,
          2 => Terminals (5).Tree_Index,
          3 => Formal_Part));

      Declaration := Tree.Add_Nonterm -- 15
        ((+declaration_ID, 2),
         (1 => Procedure_Specification,
          2 => Terminals (8).Tree_Index));

      Aspect_Specification_Opt := Tree.Add_Nonterm -- 16
        ((+aspect_specification_opt_ID, 0),
         (1 .. 0 => Invalid_Node_Index));

      Name_Opt := Tree.Add_Nonterm -- 17
        ((+name_opt_ID, 0),
         (1 .. 0 => Invalid_Node_Index));

      Package_Specification := Tree.Add_Nonterm -- 18
        ((+package_specification_ID, 0),
         (1 => 1,
          2 => 2,
          3 => Aspect_Specification_Opt,
          4 => 3,
          5 => Declaration,
          6 => Virtual_End,
          7 => Name_Opt));

      Package_Declaration := Tree.Add_Nonterm -- 19
        ((+package_declaration_ID, 0),
         (1 => Package_Specification,
          2 => Virtual_Semicolon));

      Check ("prev right_paren", Tree.Prev_Terminal (7), 6);
      Check ("prev procedure", Tree.Prev_Terminal (4), 3);
      Check ("prev left_paren", Tree.Prev_Terminal (6), 5);

      Check ("next right_paren", Tree.Next_Terminal (7), 8);
      Check ("next procedure", Tree.Next_Terminal (4), 5);
      Check ("next name", Tree.Next_Terminal (5), 6);
      Check ("next left_paren", Tree.Next_Terminal (6), 7);

      Check ("last_terminal", Tree.Last_Terminal (Package_Declaration), Virtual_Semicolon);
      Check ("prev (last)", Tree.Prev_Terminal (Virtual_Semicolon), Virtual_End);
      Check ("next end", Tree.Next_Terminal (Virtual_End), Virtual_Semicolon);
   end Test_Prev_Next_Terminal_2;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Move_Branch_Point'Access, "Test_Move_Branch_Point");
      Register_Routine (T, Test_Prev_Next_Terminal_1'Access, "Test_Prev_Next_Terminal_1");
      Register_Routine (T, Test_Prev_Next_Terminal_2'Access, "Test_Prev_Next_Terminal_2");
   end Register_Tests;

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("wisitoken-syntax_trees-test.adb");
   end Name;

end WisiToken.Syntax_Trees.Test;
--  Local Variables:
--  ada-case-strict: nil
--  End:
