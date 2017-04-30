--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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

with Ada.Text_IO;
with FastToken.Lexer;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Production;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Dragon_4_45_LALR_Test is

   --  grammar in eqn (4.21) example 4.42 pg 231

   type Token_ID is
     (
      --  terminals
      Lower_C_ID,
      Lower_D_ID,
      EOF_ID,

      --  non-terminals
      Accept_ID,
      Upper_S_ID,
      Upper_C_ID);

   First_State_Index : constant := 0;
   package Tokens_Pkg is new FastToken.Token (Token_ID, Lower_C_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Tokens_Pkg.Nonterminal;
   package Production is new FastToken.Production (Tokens_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Tokens_Pkg);
   package Parser_Root is new FastToken.Parser (Tokens_Pkg, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package Generators is new LR.LALR_Generator (EOF_ID, Production);

   --  Allow infix operators for building productions
   use type Tokens_Pkg.List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production.List.Instance;

   function "+" (Item : in Token_ID) return Tokens_Pkg.Instance'Class renames Tokens_Pkg."+";

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     Nonterminal.Get (Accept_ID) <= (+Upper_S_ID) & (+EOF_ID) + Self -- 1
     and
     Nonterminal.Get (Upper_S_ID) <= (+Upper_C_ID) & (+Upper_C_ID) + Self -- 2
     and
     Nonterminal.Get (Upper_C_ID) <= (+Lower_C_ID) & (+Upper_C_ID) + Self -- 3
     and
     Nonterminal.Get (Upper_C_ID) <= (+Lower_D_ID) + Self -- 4
     ;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, Lower_C_ID, EOF_ID, Tokens_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, First_State_Index, LR, Generators.LR1_Items, Grammar);
   use FastToken_AUnit;

   ----------
   --  Test procedures

   First : constant Generators.LR1_Items.Derivation_Matrix := Generators.LR1_Items.First
     (Grammar, Has_Empty_Production => (others => False), Trace => False);

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      --  FIRST defined in dragon pg 45

      Expected : constant Generators.LR1_Items.Derivation_Matrix :=
        (Accept_ID  => (Upper_S_ID | Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
         Upper_S_ID => (Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
         Upper_C_ID => (Lower_C_ID | Lower_D_ID => True, others => False));
   begin
      Check ("1", First, Expected);
   end Test_First;

   procedure Test_LR1_Items (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use Generators.LR1_Items;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Item_Set_List := Generators.LR1_Items.LALR_Kernels
        (Grammar, First, EOF_ID, Trace => Test.Debug, First_State_Index => First_State_Index);

      Expected : Item_Set_Ptr;

      --  [dragon] example 4.42 pg 233 shows the item sets.
      --  LR1_Items.Kernels computes the combined kernels of these (see page
      --  240). The LALR states and gotos are shown in fig 4.41 page 239.

      --  In addition, the example does a depth-first search for
      --  new sets; we do a breadth first search; so the numbering of
      --  states is different. In this test, we accomodate that by
      --  using symbolic names matching the example state labels, and
      --  pushing kernels on the list in the order they appear in the
      --  example.

      I0  : constant Item_Set_Ptr := +Get_Item_Node (Prod => 1, Lookaheads => null, Dot => 1, State => 0);
      I1  : constant Item_Set_Ptr := +Get_Item_Node (Prod => 1, Lookaheads => null, Dot => 2, State => 3);
      I2  : constant Item_Set_Ptr := +Get_Item_Node (Prod => 2, Lookaheads => null, Dot => 2, State => 4);
      I36 : constant Item_Set_Ptr := +Get_Item_Node (Prod => 3, Lookaheads => null, Dot => 2, State => 1);
      I47 : constant Item_Set_Ptr := +Get_Item_Node (Prod => 4, Lookaheads => null, Dot => 2, State => 2);
      I5  : constant Item_Set_Ptr := +Get_Item_Node (Prod => 2, Lookaheads => null, Dot => 3, State => 5);
      I89 : constant Item_Set_Ptr := +Get_Item_Node (Prod => 3, Lookaheads => null, Dot => 3, State => 6);

      Goto_0 : constant Goto_Item_Ptr :=
        (Upper_C_ID, I2, null) &
          (Upper_S_ID, I1, null) &
          (Lower_D_ID, I47, null) &
          (Lower_C_ID, I36, null);

      Goto_2 : constant Goto_Item_Ptr :=
        (Upper_C_ID, I5, null) &
          (Lower_D_ID, I47, null) &
          (Lower_C_ID, I36, null);

      Goto_36 : constant Goto_Item_Ptr :=
        (Upper_C_ID, I89, null) &
          (Lower_D_ID, I47, null) &
          (Lower_C_ID, I36, null);
   begin

      --  Goto_0:

      --  Computed has I0 last in list; push in Computed (= Set.State) order.
      Expected := new Item_Set'(I0.Set, Goto_0, I0.Set.State, Expected);
      Expected := new Item_Set'(I36.Set, Goto_List => Goto_36, State => 1, Next => Expected);
      Expected := new Item_Set'(I47.Set, Goto_List => null, State => 2, Next => Expected);
      Expected := new Item_Set'(I1.Set, Goto_List => null, State => 3, Next => Expected);
      Expected := new Item_Set'(I2.Set, Goto_List => Goto_2, State => 4, Next => Expected);
      Expected := new Item_Set'(I5.Set, Goto_List => null, State => 5, Next => Expected);
      Expected := new Item_Set'(I89.Set, Goto_List => null, State => 6, Next => Expected);

      if Test.Debug then
         Ada.Text_IO.Put_Line ("computed:");
         Generators.LR1_Items.Put (Computed);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Generators.LR1_Items.Put (Item_Set_List'(Expected, 7));
      end if;
      Check ("", Computed.Head, Expected);
   end Test_LR1_Items;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use LR;

      Test : Test_Case renames Test_Case (T);

      Computed : constant LR.Parse_Table_Ptr := Generators.Generate (Grammar, Put_Parse_Table => Test.Debug);
      Expected : LR.Parse_Table (0 .. 6);

      --  See comment in Test_LR1_Items about state numbering
      S0  : constant := 0;
      S1  : constant := 3;
      S2  : constant := 4;
      S36 : constant := 1;
      S47 : constant := 2;
      S5  : constant := 5;
      S89 : constant := 6;

   begin
      --  figure 4.41 pg 239

      Add_Action (Expected (S0), Lower_D_ID, S47);
      Add_Action (Expected (S0), Lower_C_ID, S36);
      Add_Action (Expected (S0), Tokens_Pkg.Terminal_ID'Last); -- default = error
      Add_Goto (Expected (S0), Upper_C_ID, S2);
      Add_Goto (Expected (S0), Upper_S_ID, S1);

      Add_Action (Expected (S1), EOF_ID, LR.Accept_It, Accept_ID, 1, Self);
      Add_Action (Expected (S1), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S2), Lower_D_ID, S47);
      Add_Action (Expected (S2), Lower_C_ID, S36);
      Add_Action (Expected (S2), Tokens_Pkg.Terminal_ID'Last); -- default = error
      Add_Goto (Expected (S2), Upper_C_ID, S5);

      Add_Action (Expected (S36), Lower_D_ID, S47);
      Add_Action (Expected (S36), Lower_C_ID, S36);
      Add_Action (Expected (S36), Tokens_Pkg.Terminal_ID'Last); -- default = error
      Add_Goto (Expected (S36), Upper_C_ID, S89);

      Add_Action (Expected (S47), Lower_D_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S47), Lower_C_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S47), EOF_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S47), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S5), EOF_ID, LR.Reduce, Upper_S_ID, 2, Self);
      Add_Action (Expected (S5), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S89), Lower_C_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S89), Lower_D_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S89), EOF_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S89), Tokens_Pkg.Terminal_ID'Last); -- default = error

      if Test.Debug then
         --  computed output above
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line ("expected:");
         LR.Put (Expected);
      end if;

      FastToken_AUnit.Check ("", Computed.all, Expected);
   end Parser_Table;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/dragon_4_45_lalr_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Parser_Table'Access, "debug");
      else
         Register_Routine (T, Test_First'Access, "Test_First");
         Register_Routine (T, Test_LR1_Items'Access, "Test_LR1_Items");
         Register_Routine (T, Parser_Table'Access, "Parser_Table");
      end if;
   end Register_Tests;

end Dragon_4_45_LALR_Test;
