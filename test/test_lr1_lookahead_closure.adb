--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015 Stephen Leake.  All Rights Reserved.
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

with FastToken.Lexer;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Production;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Test_LR1_Lookahead_Closure is

   --  A grammar for a greatly simplified form of the Ada 2012 case
   --  expression; this exposed a bug in Lookahead_Closure. Captured
   --  from one version of ../wisi/test/case_expression.wy.

   type Token_ID is
     (
      --  non-reporting
      Whitespace_ID,
      COMMENT_ID,

      --  terminals
      RANGE_ID,
      WHEN_ID,
      DOT_DOT_ID,
      EQUAL_GREATER_ID,
      TICK_ID,
      IDENTIFIER_ID,
      EOF_ID,

      --  non-terminals
      opentoken_accept_ID,
      case_expression_ID,
      choice_expression_ID,
      choice_relation_ID,
      discrete_choice_ID,
      factor_ID,
      factor_list_ID,
      range_nt_ID);

   package Token_Pkg is new FastToken.Token (Token_ID, RANGE_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index => 1, Nonterminal => Nonterminal);
   package LALR_Generator is new LR.LALR_Generator (Token_ID'Width, Production);

   --  Allow infix operators for building productions
   use type Token_Pkg.List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production.List.Instance;

   function "+" (Item : in Token_ID) return Token_Pkg.Instance'Class renames Token_Pkg."+";

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     --  1
     Nonterminal.Get (opentoken_accept_ID) <= Nonterminal.Get (case_expression_ID) & (+EOF_ID) + Self
     and -- 2
     Nonterminal.Get (case_expression_ID) <= (+WHEN_ID) & (+discrete_choice_ID) & (+EQUAL_GREATER_ID) + Self
     and -- 3
     Nonterminal.Get (choice_expression_ID) <= (+choice_relation_ID) + Self
     and -- 4
     Nonterminal.Get (choice_relation_ID) <= (+factor_list_ID) + Self
     and -- 5
     Nonterminal.Get (discrete_choice_ID) <= (+choice_expression_ID) + Self
     and -- 6
     Nonterminal.Get (discrete_choice_ID) <= (+range_nt_ID) + Self
     and -- 7
     Nonterminal.Get (factor_ID) <= (+IDENTIFIER_ID) + Self
     and -- 8
     Nonterminal.Get (factor_list_ID) <= (+factor_ID) + Self
     and -- 9
     Nonterminal.Get (range_nt_ID) <= (+IDENTIFIER_ID) & (+TICK_ID) & (+RANGE_ID) + Self
     and -- 10
     Nonterminal.Get (range_nt_ID) <= (+factor_list_ID) & (+DOT_DOT_ID) & (+factor_list_ID) + Self
     ;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, RANGE_ID, EOF_ID, Token_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, 1, LR, LALR_Generator, Grammar);

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use LALR_Generator.LR1;
      use FastToken_AUnit;

      Has_Empty_Production : constant Nonterminal_ID_Set := LALR_Generator.LR1.Has_Empty_Production (Grammar);

      First : constant Derivation_Matrix := First_Derivations
        (Grammar, Has_Empty_Production, Trace => Test.Debug);

      Kernels : Item_Set_List := LR0_Kernels (Grammar, First, Test.Debug, First_State_Index => 1);

      procedure Test_One (Label : in String; Input : in Item_Set; Expected : in Item_Set)
      is
         Closure : Item_Set_Ptr := new Item_Set'
           (Lookahead_Closure (Input, Has_Empty_Production, First, Grammar, Trace => Test.Debug));
      begin
         Check (Label, Closure.all, Expected);
         Free (Closure.all);
         Free (Closure);
      end Test_One;

      Null_Item_Set : constant Item_Set :=
        (Set           => null,
         Goto_List     => null,
         State         => LR.Unknown_State,
         Next          => null);

      Expected : Item_Set;
   begin
      --  Set 1: OPENTOKEN_ACCEPT_ID <= ^ CASE_EXPRESSION_ID EOF_ID
      --  closure:
      --  CASE_EXPRESSION_ID <= ^ WHEN_ID DISCRETE_CHOICE_ID EQUAL_GREATER_ID
      --  OPENTOKEN_ACCEPT_ID <= ^ CASE_EXPRESSION_ID EOF_ID
      Expected     := Null_Item_Set;
      Expected.Set := Get_Item_Node
        (Prod       => 1,
         Lookaheads => null,
         Dot        => 1,
         Next       => Expected.Set);

      Expected.Set := Get_Item_Node (2, +((1 => EOF_ID)), 1, Expected.Set);

      Test_One ("1", Find (1, Kernels).all, Expected);

      --  Set 2: CASE_EXPRESSION_ID <= WHEN_ID ^ DISCRETE_CHOICE_ID EQUAL_GREATER_ID
      Expected := Null_Item_Set;
      Expected.Set := Get_Item_Node (2, null, 2, Expected.Set);
      Expected.Set := Get_Item_Node (5, +((1 => EQUAL_GREATER_ID)), 1, Expected.Set);
      Expected.Set := Get_Item_Node (6, +((1 => EQUAL_GREATER_ID)), 1, Expected.Set);
      Expected.Set := Get_Item_Node (9, +((1 => EQUAL_GREATER_ID)), 1, Expected.Set);
      Expected.Set := Get_Item_Node (10, +((1 => EQUAL_GREATER_ID)), 1, Expected.Set);
      Expected.Set := Get_Item_Node (3, +((1 => EQUAL_GREATER_ID)), 1, Expected.Set);
      Expected.Set := Get_Item_Node (4, +((1 => EQUAL_GREATER_ID)), 1, Expected.Set);
      Expected.Set := Get_Item_Node (8, +((EQUAL_GREATER_ID, DOT_DOT_ID)), 1, Expected.Set);
      Expected.Set := Get_Item_Node (7, +((EQUAL_GREATER_ID, DOT_DOT_ID)), 1, Expected.Set);

      Test_One ("2", Find (2, Kernels).all, Expected);

      Free (Kernels);
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_lr1_lookahead_closure.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_LR1_Lookahead_Closure;
