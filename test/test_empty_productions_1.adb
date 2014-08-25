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

with AUnit.Check;
with Ada.Text_IO;
with Gen_OpenToken_AUnit;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Empty_Productions_1 is

   --  A grammar with a null production in a list (from ../wisi/test/empty_production_1.wy)

   type Token_IDs is
     (
      --  non-reporting
      Whitespace_ID,
      COMMENT_ID,

      --  terminals
      BEGIN_ID,
      IS_ID,
      SEMICOLON_ID,
      EOF_ID,

      --  non-terminals
      opentoken_accept_ID,
      declarations_ID,
      declarative_part_ID,
      body_ID);

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => BEGIN_ID,
      Last_Terminal  => EOF_ID);
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALRs is new Parsers.LALR (First_State_Index => 1);
   package LALR_Generator is new LALRs.Generator;

   --  Allow infix operators for building productions
   use type Token_Lists.Instance;
   use type Productions.Right_Hand_Side;
   use type Productions.Instance;
   use type Production_Lists.Instance;

   function "+" (Item : in Token_IDs) return Tokens_Pkg.Instance'Class renames Tokens_Pkg."+";

   Self : Nonterminals.Synthesize renames Nonterminals.Synthesize_Self;

   Grammar : constant Production_Lists.Instance :=
     Nonterminals.Get (opentoken_accept_ID) <= Nonterminals.Get (declarative_part_ID) & (+EOF_ID) -- 1
     and
     Nonterminals.Get (declarations_ID) <= (+body_ID) + Self -- 2
     and
     Nonterminals.Get (declarations_ID) <= (+declarations_ID) & (+body_ID) + Self -- 3
     and
     Nonterminals.Get (declarative_part_ID) <= +Self -- 4
     and
     Nonterminals.Get (declarative_part_ID) <= (+declarations_ID) + Self -- 5
     and
     Nonterminals.Get (body_ID) <= (+IS_ID) & (+declarative_part_ID) & (+BEGIN_ID) & (+SEMICOLON_ID) + Self -- 6
     ;

   package OpenToken_AUnit is new Gen_OpenToken_AUnit
     (Token_IDs, Tokens_Pkg, Token_Lists, Nonterminals, Productions, Production_Lists, BEGIN_ID, EOF_ID,
      Analyzers, Parsers, 1, LALRs, LALR_Generator, Grammar);

   Has_Empty_Production : constant LALR_Generator.LRk.Nonterminal_ID_Set :=
     LALR_Generator.LRk.Has_Empty_Production (Grammar);

   First : constant LALR_Generator.LRk.Derivation_Matrix := LALR_Generator.LRk.First_Derivations
     (Grammar, Has_Empty_Production, Trace => False);

   procedure Test_Goto_Transitions
     (Label    : in String;
      Kernel   : in LALR_Generator.LRk.Item_Set;
      Symbol   : in Token_IDs;
      Expected : in LALR_Generator.LRk.Item_Set;
      Debug    : in Boolean)
   is
      use Ada.Text_IO;
      use LALR_Generator.LRk;
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
      Kernels  : in LALR_Generator.LRk.Item_Set_List;
      State    : in LALRs.Unknown_State_Index;
      Expected : in LALRs.Parse_State;
      Debug    : in Boolean)
   is
      use OpenToken_AUnit;
      Accept_Index : constant                                 := 1;
      Kernel       : constant LALR_Generator.LRk.Item_Set_Ptr := LALR_Generator.LRk.Find (State, Kernels);
      Conflicts    : LALRs.Conflict_Lists.List;
      Table        : LALRs.Parse_Table (1 .. Kernels.Size);
   begin
      if Debug then
         LALR_Generator.LRk.Put (Kernel.all);
      end if;

      LALR_Generator.Add_Actions
        (Kernel, Accept_Index, Grammar, Has_Empty_Production, First, Conflicts, Table, Trace => Debug);

      Check (Label, Table (Kernel.State), Expected);
   end Test_Actions;

   ----------
   --  Test procedures

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Check;
   begin
      Check ("1", First (declarations_ID)(IS_ID), True);
   end Test_First;

   procedure Goto_Transitions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LRk;
      use OpenToken_AUnit;

      --  kernel:
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 6, -- in grammar
         Dot  => 2,
         Next => null);

      Expected : Item_Set;

   begin
      if Test.Debug then
         Ada.Text_IO.Put ("kernel 1: "); Put (Kernel);
      end if;

      --  Expected goto_transitions on IS_ID (nested declaration):
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 6, -- in Grammar
         Dot  => 2,
         Next => null);

      Test_Goto_Transitions ("1", Kernel, IS_ID, Expected, Test.Debug);

      --  Expected goto_transitions on BEGIN_ID: none

      Expected :=
        (Set       => null,
         Goto_List => null,
         State     => LALRs.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("2", Kernel, BEGIN_ID, Expected, Test.Debug);

      --  Expected goto_transitions on SEMICOLON_ID: none

      Test_Goto_Transitions ("3", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_1;

   procedure Goto_Transitions_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LRk;
      use OpenToken_AUnit;

      --  kernel:
      --  BODY_ID <= IS_ID DECLARATIVE_PART_ID ^ BEGIN_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 6, -- in grammar
         Dot  => 3,
         Next => null);

      Expected : Item_Set;

   begin
      if Test.Debug then
         Ada.Text_IO.Put ("kernel 2: "); Put (Kernel);
      end if;

      --  Expected goto_transitions on BEGIN_ID:
      --  BODY_ID <= IS_ID DECLARATIVE_PART_ID BEGIN_ID ^ SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 6, -- in Grammar
         Dot  => 4,
         Next => null);

      Test_Goto_Transitions ("1", Kernel, BEGIN_ID, Expected, Test.Debug);

      --  Expected goto_transitions on SEMICOLON_ID: none

      Expected :=
        (Set       => null,
         Goto_List => null,
         State     => LALRs.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("2", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_2;

   procedure Goto_Set_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LRk;
      use OpenToken_AUnit;

      Kernels  : constant Item_Set_List := LALR_Generator.LRk.LR0_Kernels
        (Grammar, First, Trace => False, First_State_Index => 1);
      Kernel   : constant Item_Set_Ptr  := Find (2, Kernels);
      Expected : Set_Reference_Ptr;

   begin
      --  Kernel 2:
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      --  Expected goto_list:
      --  BODY_ID => DECLARATIONS_ID <= BODY_ID ^ ; Set 5
      --  DECLARATIVE_PART_ID => BODY_ID <= IS_ID DECLARATIVE_PART_ID ^ BEGIN_ID SEMICOLON_ID ; Set 8
      --  DECLARATIONS_ID =>
      --    DECLARATIVE_PART_ID <= DECLARATIONS_ID ^
      --    DECLARATIONS_ID <= DECLARATIONS_ID ^ BODY_ID ; Set 3
      --  IS_ID => BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID ; Set 2

      --  Only Index is checked in Expected.Set
      Expected := new Set_Reference'
        (Symbol => IS_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 2, Next => null),
         Next   => Expected);

      Expected := new Set_Reference'
        (Symbol => declarations_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 3, Next => null),
         Next   => Expected);

      Expected := new Set_Reference'
        (Symbol => declarative_part_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 8, Next => null),
         Next   => Expected);

      Expected := new Set_Reference'
        (Symbol => body_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 5, Next => null),
         Next   => Expected);

      if Test.Debug then
         Ada.Text_IO.Put_Line ("kernels:"); Put (Kernels);
         Ada.Text_IO.Put_Line ("Expected:"); Put (Expected);
      end if;

      Check ("1", Kernel.Goto_List, Expected);

   end Goto_Set_1;

   procedure Actions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALRs;
      use LALR_Generator.LRk;
      use OpenToken_AUnit;

      Kernels : constant Item_Set_List := LR0_Kernels (Grammar, First, Trace => False, First_State_Index => 1);

      Expected : Parse_State;
   begin
      --  Kernel 2:
      --
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID
      --
      --  Expected actions:
      --  BEGIN_ID => reduce 0 tokens to declarative_part_id
      --  IS_ID    => shift and goto 2
      --  default  => ERROR

      --  Expected reduction gotos:
      --  DECLARATIONS_ID => Set 3
      --  DECLARATIVE_PART_ID => Set 8
      --  BODY_ID => Set 5
      Expected.Action_List := new Action_Node'
        (Symbol  => Analyzers.Terminal_ID'Last, -- ignored, since this is the last action
         Action  => new Parse_Action_Node'
           (Item => (Verb => Error),
            Next => null),
         Next  => null);

      Expected.Action_List := new Action_Node'
        (Symbol      => IS_ID,
         Action      => new Parse_Action_Node'
           (Item     =>
              (Verb  => Shift,
               State => 2),
            Next     => null),
         Next        => Expected.Action_List);

      Expected.Action_List := new Action_Node'
        (Symbol            => BEGIN_ID,
         Action            => new Parse_Action_Node'
           (Item           =>
              (Verb        => Reduce,
               LHS         => Productions.LHS (Get_Production (4)),
               Action      => null,
               Index       => 0,
               Token_Count => 0),
            Next           => null),
         Next              => Expected.Action_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => body_ID,
         State  => 5,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => declarative_part_ID,
         State  => 8,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => declarations_ID,
         State  => 3,
         Next   => Expected.Goto_List);

      Test_Actions ("1", Kernels, 2, Expected, Test.Debug);

   end Actions_1;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_1.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Actions_1'Access, "Actions_1");
      else
         Register_Routine (T, Test_First'Access, "Test_First");
         Register_Routine (T, Goto_Transitions_1'Access, "Goto_Transitions_1");
         Register_Routine (T, Goto_Transitions_2'Access, "Goto_Transitions_2");
         Register_Routine (T, Goto_Set_1'Access, "Goto_Set_1");
         Register_Routine (T, Actions_1'Access, "Actions_1");
      end if;
   end Register_Tests;

end Test_Empty_Productions_1;
