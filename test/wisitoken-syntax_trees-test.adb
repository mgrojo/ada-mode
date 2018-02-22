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

with SAL.Gen_Unbounded_Definite_Stacks;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees.AUnit;
package body WisiToken.Syntax_Trees.Test is

   --  Example tokens taken from ada_lite.wy. We don't use Ada_Lite
   --  directly, to reduce dependencies for this test.

   type Token_Enum_ID is
     (
      PROCEDURE_ID,
      SEMICOLON_ID,
      IDENTIFIER_ID,
      name_ID,
      parameter_profile_opt_ID,
      procedure_specification_ID
     );

   function "+" (Item : in Token_Enum_ID) return WisiToken.Token_ID
     is (WisiToken."+" (WisiToken.Token_ID'First, Token_Enum_ID'Pos (Item)));

   function name_0_check
    (Lexer   : in     WisiToken.Lexer.Handle;
     Nonterm : in out WisiToken.Base_Token;
     Tokens  : in     WisiToken.Base_Token_Arrays.Vector)
    return WisiToken.Semantic_Checks.Check_Status
   is
      pragma Unreferenced (Lexer);
   begin
      return WisiToken.Semantic_Checks.Propagate_Name (Nonterm, Tokens, 1);
   end name_0_check;

   function procedure_specification_0_check
    (Lexer   : in     WisiToken.Lexer.Handle;
     Nonterm : in out WisiToken.Base_Token;
     Tokens  : in     WisiToken.Base_Token_Arrays.Vector)
    return WisiToken.Semantic_Checks.Check_Status
   is
      pragma Unreferenced (Lexer);
   begin
      return WisiToken.Semantic_Checks.Propagate_Name (Nonterm, Tokens, 2);
   end procedure_specification_0_check;


   --  New stack type; holds index to Syntax_Tree instead of Token_ID
   --  FIXME: move to LR or somewhere.
   type Unknown_State_Index is new Integer range -1 .. Integer'Last;
   Unknown_State : constant Unknown_State_Index := -1;

   type Parser_Stack_Item is record
      State      : Unknown_State_Index := Unknown_State;
      Tree_Index : Node_Index          := No_Node_Index;
   end record;

   package Parser_Stacks is new SAL.Gen_Unbounded_Definite_Stacks (Parser_Stack_Item);
   --  end FIXME:

   ----------
   --  Test subprograms

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Syntax_Trees.AUnit;
      use all type Node_Arrays.Vector;
      use all type Node_Index_Arrays.Vector;

      --  Parse 'procedure Proc_1;', the same way the real parser will.
      --         |1       |10

      --  Base_Token.Name moves to Syntax_Trees.Node.Name

      Terminals : Base_Token_Arrays.Vector;
      Stack     : Parser_Stacks.Stack;
      Tree      : WisiToken.Syntax_Trees.Tree;

   begin
      --  Get tokens, push on stack and tree until we can reduce
      --  States from ada_lite.parse_table
      Stack.Push ((0, No_Node_Index));

      Terminals.Append ((+PROCEDURE_ID, (1, 9), others => <>));
      Stack.Push ((3, Tree.Add_Terminal (Terminal => Terminals.Last_Index))); -- 1

      Terminals.Append ((+IDENTIFIER_ID, (11, 16), others => <>));
      Stack.Push ((18, Tree.Add_Terminal (Terminal => Terminals.Last_Index))); -- 2

      Terminals.Append ((+SEMICOLON_ID, (17, 17), others => <>));

      --  name : IDENTIFIER ()(propagate_name)
      declare
         Nonterm : constant Node_Index := Tree.Add_Nonterm -- 3
           (Nonterm => +name_ID,
            Check   => name_0_check'Access);
      begin
         Tree.Set_Child (Parent => Nonterm, Child => Stack.Pop.Tree_Index);
         Stack.Push ((19, Nonterm));
         --  Emulate calling name_0_check, with Name in Tree.
         Tree.Nodes (Nonterm).Name := (11, 16);
      end;

      --  parameter_profile_opt : ;; empty
      declare
         Nonterm : constant Node_Index := Tree.Add_Nonterm (+parameter_profile_opt_ID); -- 4
      begin
         Stack.Push ((35, Nonterm));
      end;

      --  procedure_specification : PROCEDURE name parameter_profile_opt
      declare
         Nonterm : constant Node_Index := Tree.Add_Nonterm -- 5
           (+procedure_specification_ID,
            Check => procedure_specification_0_check'Access);
         Children : Node_Index_Arrays.Vector;
      begin
         Children.Prepend (Stack.Pop.Tree_Index); -- parameter_profile_opt
         Children.Prepend (Stack.Pop.Tree_Index); -- name
         Children.Prepend (Stack.Pop.Tree_Index); -- PROCEDURE

         Tree.Set_Children (Nonterm, Children);
         Stack.Push ((10, Nonterm));

         --  Emulate calling procedure_specification_0_check, with Name in Tree.
         Tree.Nodes (Nonterm).Name := (11, 16);
      end;

      Check
        ("1",
         Tree,
         ((Root => 0, -- FIXME: not clear whether we need Root

           Nodes        =>
             ((Parent   => 5, Terminal => 1, others => <>) &                                                       -- 1
                (Parent => 3, Terminal => 2, others => <>) &                                                       -- 2
                (Parent => 5, Nonterm => +name_ID, Children => +2, Check   => name_0_check'Access, others => <>) & -- 3
                (Parent => 5, Nonterm => +parameter_profile_opt_ID, others => <>) &                                -- 4
                (Parent => 0, Nonterm => +procedure_specification_ID, Children => 1 & 3 & 4,
                 Check  => procedure_specification_0_check'Access, others => <>)                                   -- 5
             ))));

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
