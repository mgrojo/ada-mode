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

with WisiToken.Syntax_Trees.AUnit_Private;
with WisiToken.Syntax_Trees.AUnit_Public;
package body WisiToken.Syntax_Trees.Test is

   --  Example tokens taken from ada_lite.wy. We don't use Ada_Lite
   --  directly, to reduce dependencies for this test.

   type Token_Enum_ID is
     (
      PROCEDURE_ID,
      IDENTIFIER_ID,
      name_ID,
      parameter_profile_opt_ID,
      procedure_specification_ID
     );

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

   ----------
   --  Test subprograms

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Syntax_Trees.AUnit_Private;
      use WisiToken.Syntax_Trees.AUnit_Public;
      use all type Node_Arrays.Vector;
      use all type Valid_Node_Index_Arrays.Vector;

      Terminals  : aliased Protected_Base_Token_Arrays.Vector;
      Tree       : WisiToken.Syntax_Trees.Tree;
      Node_Proc  : Node_Index;
      Node_Ident : Node_Index;
      Node_Name  : Node_Index;
      Node_Prof  : Node_Index;
   begin
      Tree.Initialize (Terminals'Unchecked_Access);

      Terminals.Append ((+PROCEDURE_ID, (1, 9)));
      Node_Proc := Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 1

      Terminals.Append ((+IDENTIFIER_ID, (11, 16)));
      Node_Ident := Tree.Add_Terminal (Terminal => Terminals.Last_Index); -- 2

      Node_Name := Tree.Add_Nonterm (+name_ID, null, 1, 0); -- 3
      Tree.Set_Children (Parent => Node_Name, Children => (1 => Node_Ident));
      Tree.Set_Name_Region (Node_Name, (11, 16));

      Node_Prof := Tree.Add_Nonterm (+parameter_profile_opt_ID, null, 1, 0); -- 4

      --  procedure_specification : PROCEDURE name parameter_profile_opt
      declare
         Nonterm : constant Node_Index := Tree.Add_Nonterm (+procedure_specification_ID, null, 1, 0); -- 5

         Children : constant Valid_Node_Index_Array := (Node_Proc, Node_Name, Node_Prof);
      begin
         Tree.Set_Children (Nonterm, Children);
      end;

      Check
        ("1",
         Tree,
         ((Ada.Finalization.Controlled with
           Terminals        => Terminals'Unchecked_Access,
           Nodes            =>
             ((Shared_Terminal,
               Parent       => 5,
               Terminal     => 1) &  -- 1
                (Shared_Terminal,
                 Parent     => 3,
                 Terminal   => 2) &  -- 2
                (Nonterm,
                 Parent     => 5,
                 Nonterm_ID => +name_ID,
                 Children   => +2,
                 others     => <>) & -- 3
                (Nonterm,
                 Parent     => 5,
                 Nonterm_ID => +parameter_profile_opt_ID,
                 Virtual    => True,
                 others     => <>) & -- 4
                (Nonterm,
                 Parent     => 0,
                 Nonterm_ID => +procedure_specification_ID,
                 Children   => 1 & 3 & 4,
                 others     => <>)   -- 5
             ),
           Traversing => False,
           Augmented_Present => False)));

   end Nominal;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use Standard.AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding function Name (T : Test_Case) return Standard.AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("wisitoken-syntax_trees-test.adb");
   end Name;

end WisiToken.Syntax_Trees.Test;
