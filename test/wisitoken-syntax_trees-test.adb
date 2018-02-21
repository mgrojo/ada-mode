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


   ----------
   --  Test subprograms

   procedure Nominal (T : in out Standard.AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.Syntax_Trees.AUnit;
      use all type Node_Arrays.Vector;
      use all type Node_Index_Arrays.Vector;

      Terminals      : Base_Token_Arrays.Vector;
      Tree           : WisiToken.Syntax_Trees.Tree;
      Procedure_I    : Positive_Index_Type;
      Procedure_Cur  : Cursor;
      Identifier_I   : Positive_Index_Type;
      Identifier_Cur : Cursor;
      Name_Cur       : Cursor;
      Profile_Cur    : Cursor;
      Semicolon_I    : Positive_Index_Type;
      Semicolon_Cur  : Cursor;
      Proc_Spec_Cur  : Cursor;
      pragma Unreferenced (Semicolon_Cur);
   begin
      --  Parse 'procedure Proc_1;'
      --         |1       |10
      Terminals.Append ((+PROCEDURE_ID, (1, 9), others => <>));

      Procedure_I   := Terminals.Last_Index;
      Procedure_Cur := Tree.Add_Terminal (Terminal => Procedure_I);

      Terminals.Append ((+IDENTIFIER_ID, (11, 16), others => <>));

      Identifier_I   := Terminals.Last_Index;
      Identifier_Cur := Tree.Add_Terminal (Terminal => Identifier_I);

      Terminals.Append ((+SEMICOLON_ID, (17, 17), others => <>));

      Semicolon_I   := Terminals.Last_Index;
      Semicolon_Cur := Tree.Add_Terminal (Terminal => Semicolon_I);

      --  name : IDENTIFIER ()(propagate_name)
      Name_Cur := Tree.Add_Nonterm
        (Nonterm => +name_ID,
         Check   => name_0_check'Access);
      Tree.Set (Parent => Name_Cur, Child => Identifier_Cur);

      --  parameter_profile_opt : ;; empty
      Profile_Cur := Tree.Add_Nonterm (+parameter_profile_opt_ID);

      --  procedure_specification : PROCEDURE name parameter_profile_opt
      Proc_Spec_Cur := Tree.Add_Nonterm (+procedure_specification_ID, Check => procedure_specification_0_check'Access);
      Tree.Set (Proc_Spec_Cur, Child => Procedure_Cur);
      Tree.Set (Proc_Spec_Cur, Child => Name_Cur);
      Tree.Set (Proc_Spec_Cur, Child => Profile_Cur);

      Check
        ("1",
         Tree,
         ((Root => 0, -- FIXME: not clear whether we need Root

           Nodes        =>
             ((Parent   => 6, Terminal => Procedure_I, others => <>) &                                             -- 1
                (Parent => 4, Terminal => Identifier_I, others => <>) &                                            -- 2
                (Parent => 0, Terminal => Semicolon_I, others => <>) &                                             -- 3
                (Parent => 6, Nonterm => +name_ID, Children => +2, Check   => name_0_check'Access, others => <>) & -- 4
                (Parent => 6, Nonterm => +parameter_profile_opt_ID, others => <>) &                                -- 5
                (Parent => 0, Nonterm => +procedure_specification_ID, Children => 1 & 4 & 5,
                 Check  => procedure_specification_0_check'Access, others => <>)                                   -- 6
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
