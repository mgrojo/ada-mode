--  Abstract:
--
--  See spec
--
--  Copyright (C) 2018 Stephen Leake
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

with WisiToken.AUnit;
with WisiToken.Syntax_Trees.AUnit_Private;
with WisiToken.Syntax_Trees.AUnit_Public;
with WisiToken.Syntax_Trees.Branched.AUnit_Private;
package body WisiToken.Syntax_Trees.Branched.Test is

   --  Example tokens taken from ada_lite.wy. We don't use Ada_Lite
   --  directly, to reduce dependencies for this test.

   type Token_Enum_ID is
     (
      PROCEDURE_ID,
      IDENTIFIER_ID,
      name_ID
     );

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

   ----------
   --  Test subprograms

   procedure Test_Move_Branch_Point (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.Branched.AUnit_Private;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use all type Node_Arrays.Vector;
      use all type Valid_Node_Index_Arrays.Vector;

      Terminals     : aliased Protected_Base_Token_Arrays.Vector;
      Shared_Tree   : aliased WisiToken.Syntax_Trees.Tree;
      Branched_Tree : WisiToken.Syntax_Trees.Branched.Tree;
      Junk          : Node_Index;
      pragma Unreferenced (Junk);
      Node_Ident_1  : Node_Index;
      Node_Ident_2  : Node_Index;
      Node_Name     : Node_Index;

      Expected_Branched_Nodes : Node_Arrays.Vector;
   begin
      --  Create a branched tree, set a child of a new node to a shared
      --  node, thus invoking Move_Branch_Point.

      Shared_Tree.Initialize (Terminals'Unchecked_Access);

      Terminals.Append ((+PROCEDURE_ID, (1, 9)));
      Junk := Shared_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 1

      Terminals.Append ((+IDENTIFIER_ID, (11, 16)));
      Node_Ident_1 := Shared_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 2

      Terminals.Append ((+IDENTIFIER_ID, (18, 19)));
      Junk := Shared_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 3

      Branched_Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => False);

      Node_Name := Branched_Tree.Add_Nonterm (+name_ID, null, 1, 0); -- 4
      Branched_Tree.Set_Children (Parent => Node_Name, Children => (1 => Node_Ident_1));
      --  moves branch point; Branched_Tree.Last_Shared_Node is now 1.

      Check ("node 4", Node_Name, 4);

      Terminals.Append ((+IDENTIFIER_ID, (21, 22)));
      Node_Ident_2 := Branched_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 5

      Check ("node 5", Node_Ident_2, 5);

      Expected_Branched_Nodes.Set_First (2);
      Expected_Branched_Nodes.Append
        ((Shared_Terminal,
          Parent       => 4,
          Terminal     => 2));  -- 2

      Expected_Branched_Nodes.Append
        ((Shared_Terminal,
          Parent     => 0,
          Terminal   => 3));  -- 3

      Expected_Branched_Nodes.Append
        ((Nonterm,
          Parent     => 0,
          Nonterm_ID => +name_ID,
          Children   => +2,
          others     => <>)); -- 4

      Expected_Branched_Nodes.Append
        ((Shared_Terminal,
          Parent     => 0,
          Terminal   => 4));    -- 5

      Check
        ("branched tree",
         Branched_Tree,
         ((Ada.Finalization.Controlled with
           Shared_Tree      => Shared_Tree'Unchecked_Access,
           Last_Shared_Node => 1,
           Branched_Nodes   => Expected_Branched_Nodes,
           Flush            => False)));

      Check ("nodes 2", Branched_Tree.Base_Token (2), Shared_Tree.Base_Token (2));
      Check ("nodes 3", Branched_Tree.Base_Token (3), Shared_Tree.Base_Token (3));
   end Test_Move_Branch_Point;

   procedure Test_Delete_Flushed (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.AUnit;
      use WisiToken.Syntax_Trees.Branched.AUnit_Private;
      use WisiToken.Syntax_Trees.AUnit_Private;
      use all type Node_Arrays.Vector;
      use all type Valid_Node_Index_Arrays.Vector;

      Terminals     : aliased Protected_Base_Token_Arrays.Vector;
      Shared_Tree   : aliased WisiToken.Syntax_Trees.Tree;
      Branched_Tree : WisiToken.Syntax_Trees.Branched.Tree;
      Junk          : Node_Index;
      pragma Unreferenced (Junk);
      Ident_1 : Node_Index;
      Ident_2 : Node_Index;
      Parent : Node_Index;

      Expected_Nodes : Node_Arrays.Vector;
   begin
      --  Create a flushed branched tree, delete a nonterm and its children.

      Shared_Tree.Initialize (Terminals'Unchecked_Access);

      Terminals.Append ((+PROCEDURE_ID, (1, 9)));
      Junk := Shared_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 1

      Terminals.Append ((+IDENTIFIER_ID, (11, 16)));
      Ident_1 := Shared_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 2

      Terminals.Append ((+IDENTIFIER_ID, (18, 19)));
      Ident_2 := Shared_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 3

      Branched_Tree.Initialize (Shared_Tree'Unchecked_Access, Flush => True);

      Parent := Branched_Tree.Add_Nonterm (+name_ID, null, 1, 0); -- 4
      Branched_Tree.Set_Children (Parent => Parent, Children => (1 => Ident_1, 2 => Ident_2));

      Terminals.Append ((+IDENTIFIER_ID, (21, 22)));
      Junk := Branched_Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 5

      Branched_Tree.Delete (Parent);

      Expected_Nodes.Set_First (1);
      Expected_Nodes.Set_Last (5);

      Expected_Nodes.Replace_Element (1, (Shared_Terminal, Parent   => No_Node_Index, Terminal => 1));

      for I in Node_Index'(2) .. 4 loop
         Expected_Nodes.Replace_Element (I, (Empty, Parent => No_Node_Index));
      end loop;

      Expected_Nodes.Replace_Element (5, (Shared_Terminal, Parent => No_Node_Index, Terminal => 4));

      Check ("nodes", Shared_Tree.Nodes, Expected_Nodes);

   end Test_Delete_Flushed;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Move_Branch_Point'Access, "Test_Move_Branch_Point");
      Register_Routine (T, Test_Delete_Flushed'Access, "Test_Delete_Flushed");
   end Register_Tests;

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("wisitoken-syntax_trees-branched-test.adb");
   end Name;

end WisiToken.Syntax_Trees.Branched.Test;
