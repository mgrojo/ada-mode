--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013, 2014, 2015 Stephen Leake.  All Rights Reserved.
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
package body Test_Empty_Productions_7 is

   --  A grammar with two consecutive possibly empty productions (from
   --  ../wisi/test/empty_production_7.wy)

   type Token_ID is
     (
      --  non-reporting
      Whitespace_ID,
      COMMENT_ID,

      --  terminals
      ALIASED_ID,
      CONSTANT_ID,
      SEMICOLON_ID,
      IDENTIFIER_ID,
      EOF_ID,

      --  non-terminals
      object_declaration_list_ID,
      object_declaration_ID,
      aliased_opt_ID,
      constant_opt_ID,
      opentoken_accept_ID);

   First_State_Index : constant Integer := 1;

   package Token_Pkg is new FastToken.Token (Token_ID, ALIASED_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package LALR_Generator is new LR.LALR_Generator (EOF_ID, Production);

   --  Allow infix operators for building productions
   use type Token_Pkg.List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production.List.Instance;

   function "+" (Item : in Token_ID) return Token_Pkg.Instance'Class renames Token_Pkg."+";

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     Nonterminal.Get (opentoken_accept_ID) <= (+object_declaration_list_ID) & (+EOF_ID) + Self -- 1
     and
     Nonterminal.Get (object_declaration_list_ID) <= (+object_declaration_ID) + Self -- 2
     and
     Nonterminal.Get (object_declaration_list_ID) <= (+object_declaration_list_ID) & (+object_declaration_ID) +
     Self -- 3
     and
     Nonterminal.Get (object_declaration_ID) <= (+IDENTIFIER_ID) & (+aliased_opt_ID) & (+constant_opt_ID) &
     (+SEMICOLON_ID) + Self -- 4
     and
     Nonterminal.Get (aliased_opt_ID) <= +Self -- 5
     and
     Nonterminal.Get (aliased_opt_ID) <= (+ALIASED_ID) + Self -- 6
     and
     Nonterminal.Get (constant_opt_ID) <= +Self -- 7
     and
     Nonterminal.Get (constant_opt_ID) <= (+CONSTANT_ID) + Self -- 8
     ;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, ALIASED_ID, EOF_ID, EOF_ID, Token_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, First_State_Index, LR, LALR_Generator, Grammar);

   Has_Empty_Production : constant LALR_Generator.LR1_Items.Nonterminal_ID_Set :=
     LALR_Generator.LR1_Items.Has_Empty_Production (Grammar);

   First : constant LALR_Generator.LR1_Items.Derivation_Matrix := LALR_Generator.LR1_Items.First_Derivations
     (Grammar, Has_Empty_Production, Trace => False);

   procedure Test_Goto_Transitions
     (Label    : in String;
      Kernel   : in LALR_Generator.LR1_Items.Item_Set;
      Symbol   : in Token_ID;
      Expected : in LALR_Generator.LR1_Items.Item_Set;
      Debug    : in Boolean)
   is
      use Ada.Text_IO;
      use LALR_Generator.LR1_Items;
      use FastToken_AUnit;
      Computed : constant Item_Set := Goto_Transitions (Kernel, Symbol, First, Grammar);
   begin
      if Debug then
         Put_Line ("symbol:   " & Token_ID'Image (Symbol));
         Put ("expected: "); Put (Expected);
         Put ("computed: "); Put (Computed);
      end if;

      Check (Label, Computed, Expected);
   end Test_Goto_Transitions;

   procedure Test_Actions
     (Label    : in String;
      Kernels  : in LALR_Generator.LR1_Items.Item_Set_List;
      State    : in LR.State_Index;
      Expected : in LR.Parse_State;
      Debug    : in Boolean)
   is
      use FastToken_AUnit;
      Kernel    : constant LALR_Generator.LR1_Items.Item_Set_Ptr := LALR_Generator.LR1_Items.Find (State, Kernels);
      Conflicts : LR.Conflict_Lists.List;
      Table     : LR.Parse_Table (1 .. LR.State_Index (Kernels.Size));
   begin
      LALR_Generator.Add_Actions
        (Kernel, Grammar, Has_Empty_Production, First, Conflicts, Table, Trace => Debug);

      if Debug then
         Ada.Text_IO.Put_Line ("Computed:");
         LR.Put (Table (Kernel.State));
         Ada.Text_IO.Put_Line ("Expected:");
         LR.Put (Expected);
      end if;

      Check (Label, Table (Kernel.State), Expected);
   end Test_Actions;

   ----------
   --  Test procedures

   procedure Test_Lookahead_Closure (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LR1_Items;
      use FastToken_AUnit;

      --  kernel:
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ^ ALIASED_OPT_ID CONSTANT_OPT_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 4, -- in grammar
         Dot  => 2,
         Next => null);

      Closure : constant Item_Set := LALR_Generator.LR1_Items.Lookahead_Closure
        (Kernel, Has_Empty_Production, First, Grammar, Trace => Test.Debug);

      Expected_Set : Item_Ptr;
      Expected     : Item_Set;
   begin
      --  Expected lookahead closure:
      --
      --  ALIASED_OPT_ID <= ^ ALIASED_ID, CONSTANT_ID/SEMICOLON_ID
      --  ALIASED_OPT_ID <= ^, CONSTANT_ID/SEMICOLON_ID
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ^ ALIASED_OPT_ID CONSTANT_OPT_ID SEMICOLON_ID, <empty>

      Expected_Set := Get_Item_Node
        (Prod       => 4,
         Lookaheads => null,
         Dot        => 2,
         Next       => null);

      Expected_Set := Get_Item_Node
        (Prod       => 5,
         Lookaheads => +(SEMICOLON_ID, CONSTANT_ID),
         Dot        => 1,
         Next       => Expected_Set);

      Expected_Set := Get_Item_Node
        (Prod       => 6,
         Lookaheads => +(SEMICOLON_ID, CONSTANT_ID),
         Dot        => 1,
         Next       => Expected_Set);

      Expected :=
        (Set       => Expected_Set,
         Goto_List => null,
         State     => LR.Unknown_State,
         Next      => null);

      if Test.Debug then
         --  computed output by Lookahead_Closure
         Ada.Text_IO.Put_Line ("Expected:");
         LALR_Generator.LR1_Items.Put (Expected);
         Ada.Text_IO.New_Line;
      end if;

      Check ("1", Closure, Expected);
   end Test_Lookahead_Closure;

   procedure Goto_Transitions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LR1_Items;
      use FastToken_AUnit;

      --  kernel:
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ^ ALIASED_OPT_ID CONSTANT_OPT_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 4, -- in grammar
         Dot  => 2,
         Next => null);

      Expected : Item_Set;

   begin
      if Test.Debug then
         Ada.Text_IO.Put ("kernel: "); Put (Kernel);
      end if;

      --  Expected goto_transitions on ALIASED_ID:
      --  ALIASED_OPT_ID <= ALIASED_ID ^

      Expected := Get_Item_Set
        (Prod => 6,
         Dot  => 2,
         Next => null);

      Test_Goto_Transitions ("1", Kernel, ALIASED_ID, Expected, Test.Debug);

      --  Expected goto_transitions on aliased_opt_ID:
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ALIASED_OPT_ID ^ CONSTANT_OPT_ID SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 4,
         Dot  => 3,
         Next => null);

      Test_Goto_Transitions ("2", Kernel, aliased_opt_ID, Expected, Test.Debug);

      --  Expected goto_transitions on CONSTANT_ID, SEMICOLON_ID, constant_opt_ID: none

      Expected :=
        (Set       => null,
         Goto_List => null,
         State     => LR.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("3", Kernel, CONSTANT_ID, Expected, Test.Debug);
      Test_Goto_Transitions ("4", Kernel, SEMICOLON_ID, Expected, Test.Debug);
      Test_Goto_Transitions ("5", Kernel, constant_opt_ID, Expected, Test.Debug);

   end Goto_Transitions_1;

   procedure Goto_Transitions_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LR1_Items;
      use FastToken_AUnit;

      --  kernel:
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ALIASED_OPT_ID ^ CONSTANT_OPT_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 4, -- in grammar
         Dot  => 3,
         Next => null);

      Expected : Item_Set;

   begin
      if Test.Debug then
         Ada.Text_IO.Put ("kernel: "); Put (Kernel);
      end if;

      --  Expected goto_transitions on CONSTANT_ID:
      --   CONSTANT_OPT_ID <= CONSTANT_ID ^

      Expected := Get_Item_Set
        (Prod => 8,
         Dot  => 2,
         Next => null);

      Test_Goto_Transitions ("1", Kernel, CONSTANT_ID, Expected, Test.Debug);

      --  Expected goto_transitions on constant_opt_ID:
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ALIASED_OPT_ID CONSTANT_OPT_ID ^ SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 4,
         Dot  => 4,
         Next => null);

      Test_Goto_Transitions ("2", Kernel, constant_opt_ID, Expected, Test.Debug);

      --  Expected goto_transitions on SEMICOLON_ID: none

      Expected :=
        (Set       => null,
         Goto_List => null,
         State     => LR.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("3", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_2;

   procedure Actions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR;
      use LALR_Generator;
      use LALR_Generator.LR1_Items;
      use FastToken_AUnit;

      Used_Tokens : Token_Pkg.Token_Array_Boolean := (others => False);

      Accept_Index : constant := 5;

      Kernels : Item_Set_List := LALR_Generator.LR1_Items.Kernels
        (Grammar, First, EOF_ID, Trace => Test.Debug, First_State_Index => LR.Unknown_State_Index (First_State_Index));

      Expected : Parse_State;
   begin
      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First, Kernels, Accept_Index, Used_Tokens, Test.Debug);

      if Test.Debug then
         LALR_Generator.LR1_Items.Put (Kernels);
      end if;

      --  kernel 2:
      --
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ^ ALIASED_OPT_ID CONSTANT_OPT_ID SEMICOLON_ID
      --
      --  Expected actions:
      --  CONSTANT_ID => reduce 0 tokens to ALIASED_OPT_ID
      --  SEMICOLON_ID => reduce 0 tokens to ALIASED_OPT_ID
      --  ALIASED_ID => shift and goto ALIASED_OPT_ID <= ALIASED_ID ^ ; 7
      --  default  => ERROR

      --  Expected reduction gotos:
      --  aliased_opt_id => OBJECT_DECLARATION_ID <= IDENTIFIER_ID ALIASED_OPT_ID ^ CONSTANT_OPT_ID SEMICOLON_ID ; 8

      Expected.Action_List := new Action_Node'
        (Symbol  => Token_Pkg.Terminal_ID'Last, -- ignored, since this is the last action
         Action  => new Parse_Action_Node'
           (Item => (Verb => Error),
            Next => null),
         Next  => null);

      Expected.Action_List := new Action_Node'
        (Symbol      => ALIASED_ID,
         Action      => new Parse_Action_Node'
           (Item     =>
              (Verb  => Shift,
               State => 7),
            Next     => null),
         Next        => Expected.Action_List);

      Expected.Action_List := new Action_Node'
        (Symbol            => SEMICOLON_ID,
         Action            => new Parse_Action_Node'
           (Item           =>
              (Verb        => Reduce,
               LHS         => Get_Production (5).LHS,
               Action      => null,
               Index       => 0,
               Token_Count => 0),
            Next           => null),
         Next              => Expected.Action_List);

      Expected.Action_List := new Action_Node'
        (Symbol            => CONSTANT_ID,
         Action            => new Parse_Action_Node'
           (Item           =>
              (Verb        => Reduce,
               LHS         => Get_Production (5).LHS,
               Action      => null,
               Index       => 0,
               Token_Count => 0),
            Next           => null),
         Next              => Expected.Action_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => aliased_opt_ID,
         State  => 8,
         Next   => null);

      Test_Actions ("1", Kernels, 2, Expected, Test.Debug);

   end Actions_1;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_7.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Goto_Transitions_2'Access, "debug");
      else
         Register_Routine (T, Test_Lookahead_Closure'Access, "Test_Lookahead_Closure");
         Register_Routine (T, Goto_Transitions_1'Access, "Goto_Transitions_1");
         Register_Routine (T, Goto_Transitions_2'Access, "Goto_Transitions_2");
         Register_Routine (T, Actions_1'Access, "Actions_1");
      end if;
   end Register_Tests;

end Test_Empty_Productions_7;
