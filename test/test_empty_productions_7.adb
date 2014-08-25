--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013, 2014 Stephen Leake.  All Rights Reserved.
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
with Gen_OpenToken_AUnit;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Empty_Productions_7 is

   --  A grammar with two consecutive possibly empty productions (from
   --  ../wisi/test/empty_production_7.wy)

   type Token_IDs is
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

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => ALIASED_ID,
      Last_Terminal  => EOF_ID);
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALRs is new Parsers.LALR (First_State_Index);
   package LALR_Generators is new LALRs.Generator;

   --  Allow infix operators for building productions
   use type Token_Lists.Instance;
   use type Productions.Right_Hand_Side;
   use type Productions.Instance;
   use type Production_Lists.Instance;

   function "+" (Item : in Token_IDs) return Tokens_Pkg.Instance'Class renames Tokens_Pkg."+";

   Self : Nonterminals.Synthesize renames Nonterminals.Synthesize_Self;

   Grammar : constant Production_Lists.Instance :=
     Nonterminals.Get (opentoken_accept_ID) <= (+object_declaration_list_ID) & (+EOF_ID) -- 1
     and
     Nonterminals.Get (object_declaration_list_ID) <= (+object_declaration_ID) + Self -- 2
     and
     Nonterminals.Get (object_declaration_list_ID) <= (+object_declaration_list_ID) & (+object_declaration_ID) +
     Self -- 3
     and
     Nonterminals.Get (object_declaration_ID) <= (+IDENTIFIER_ID) & (+aliased_opt_ID) & (+constant_opt_ID) &
     (+SEMICOLON_ID) + Self -- 4
     and
     Nonterminals.Get (aliased_opt_ID) <= +Self -- 5
     and
     Nonterminals.Get (aliased_opt_ID) <= (+ALIASED_ID) + Self -- 6
     and
     Nonterminals.Get (constant_opt_ID) <= +Self -- 7
     and
     Nonterminals.Get (constant_opt_ID) <= (+CONSTANT_ID) + Self -- 8
     ;

   package OpenToken_AUnit is new Gen_OpenToken_AUnit
     (Token_IDs, Tokens_Pkg, Token_Lists, Nonterminals, Productions, Production_Lists, ALIASED_ID, EOF_ID,
      Analyzers, Parsers, First_State_Index, LALRs, LALR_Generators, Grammar);

   Has_Empty_Production : constant LALR_Generators.LRk.Nonterminal_ID_Set :=
     LALR_Generators.LRk.Has_Empty_Production (Grammar);

   First : constant LALR_Generators.LRk.Derivation_Matrix := LALR_Generators.LRk.First_Derivations
     (Grammar, Has_Empty_Production, Trace => False);

   Accept_Index : constant := 5;

   procedure Test_Goto_Transitions
     (Label    : in String;
      Kernel   : in LALR_Generators.LRk.Item_Set;
      Symbol   : in Token_IDs;
      Expected : in LALR_Generators.LRk.Item_Set;
      Debug    : in Boolean)
   is
      use Ada.Text_IO;
      use LALR_Generators.LRk;
      use OpenToken_AUnit;
      Computed : constant Item_Set := Goto_Transitions (Kernel, Symbol, First, Grammar);
   begin
      if Debug then
         Put_Line ("symbol:   " & Token_IDs'Image (Symbol));
         Put ("expected: "); Put (Expected);
         Put ("computed: "); Put (Computed);
      end if;

      Check (Label, Computed, Expected);
   end Test_Goto_Transitions;

   procedure Test_Actions
     (Label    : in String;
      Kernels  : in LALR_Generators.LRk.Item_Set_List;
      State    : in LALRs.State_Index;
      Expected : in LALRs.Parse_State;
      Debug    : in Boolean)
   is
      use OpenToken_AUnit;
      Kernel    : constant LALR_Generators.LRk.Item_Set_Ptr := LALR_Generators.LRk.Find (State, Kernels);
      Conflicts : LALRs.Conflict_Lists.List;
      Table     : LALRs.Parse_Table (1 .. LALRs.State_Index (Kernels.Size));
   begin
      LALR_Generators.Add_Actions
        (Kernel, Accept_Index, Grammar, Has_Empty_Production, First, Conflicts, Table, Trace => Debug);

      if Debug then
         Ada.Text_IO.Put_Line ("Computed:");
         LALR_Generators.Put (Table (Kernel.State));
         Ada.Text_IO.Put_Line ("Expected:");
         LALR_Generators.Put (Expected);
      end if;

      Check (Label, Table (Kernel.State), Expected);
   end Test_Actions;

   ----------
   --  Test procedures

   procedure Test_Lookahead_Closure (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generators.LRk;
      use OpenToken_AUnit;

      --  kernel:
      --  OBJECT_DECLARATION_ID <= IDENTIFIER_ID ^ ALIASED_OPT_ID CONSTANT_OPT_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 4, -- in grammar
         Dot  => 2,
         Next => null);

      Closure : constant Item_Set := LALR_Generators.LRk.Lookahead_Closure
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
         State     => LALRs.Unknown_State,
         Next      => null);

      if Test.Debug then
         --  computed output by Lookahead_Closure
         Ada.Text_IO.Put_Line ("Expected:");
         LALR_Generators.LRk.Put (Expected);
         Ada.Text_IO.New_Line;
      end if;

      Check ("1", Closure, Expected);
   end Test_Lookahead_Closure;

   procedure Goto_Transitions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generators.LRk;
      use OpenToken_AUnit;

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
         State     => LALRs.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("3", Kernel, CONSTANT_ID, Expected, Test.Debug);
      Test_Goto_Transitions ("4", Kernel, SEMICOLON_ID, Expected, Test.Debug);
      Test_Goto_Transitions ("5", Kernel, constant_opt_ID, Expected, Test.Debug);

   end Goto_Transitions_1;

   procedure Goto_Transitions_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generators.LRk;
      use OpenToken_AUnit;

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
         State     => LALRs.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("3", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_2;

   procedure Actions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALRs;
      use LALR_Generators;
      use LALR_Generators.LRk;
      use OpenToken_AUnit;

      Used_Tokens : Analyzers.Token_Array_Boolean := (others => False);

      Kernels : Item_Set_List := LR0_Kernels
        (Grammar, First, Trace => Test.Debug, First_State_Index => LALRs.Unknown_State_Index (First_State_Index));

      Expected : Parse_State;
   begin
      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First, Kernels, Accept_Index, Used_Tokens, Test.Debug);

      if Test.Debug then
         LALR_Generators.LRk.Put (Kernels);
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
        (Symbol  => Analyzers.Terminal_ID'Last, -- ignored, since this is the last action
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
               LHS         => Productions.LHS (Get_Production (5)),
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
               LHS         => Productions.LHS (Get_Production (5)),
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
