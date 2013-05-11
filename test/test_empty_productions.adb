--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013 Stephen Leake.  All Rights Reserved.
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
with OpenToken.Production.Parser.LRk_Item;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Empty_Productions is

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
      declarative_part_ID,
      declarations_ID,
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
   package LR1 is new Parsers.LRk_Item (1);

   --  Allow infix operators for building productions
   use type Token_Lists.Instance;
   use type Productions.Right_Hand_Side;
   use type Productions.Instance;
   use type Production_Lists.Instance;

   function "+"
     (Item : in Token_IDs)
     return Tokens_Pkg.Instance'Class
   renames Tokens_Pkg."+";

   Self : Nonterminals.Synthesize renames Nonterminals.Synthesize_Self;

   Grammar : constant Production_Lists.Instance :=
     Nonterminals.Get (opentoken_accept_ID) <= Nonterminals.Get (declarations_ID) & (+EOF_ID) -- 1
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

   package LR1_AUnit is new Gen_OpenToken_AUnit
     (Token_IDs, Tokens_Pkg, Token_Lists, Nonterminals, Productions, Production_Lists, BEGIN_ID, EOF_ID,
      Analyzers, Parsers, LR1, Grammar);

   First : constant LR1.Derivation_Matrix := LR1.First_Derivations (Grammar, Trace => False);

   procedure Test_Goto_Transitions
     (Label    : in String;
      Kernel   : in LR1.Item_Set;
      Symbol   : in Token_IDs;
      Expected : in LR1.Item_Set;
      Debug    : in Boolean)
   is
      use Ada.Text_IO;
      use LR1;
      use LR1_AUnit;
      Computed : constant Item_Set := Goto_Transitions (Kernel, Symbol, First, Grammar);
   begin
      if Debug then
         Put_Line ("symbol:   " & Token_IDs'Image (Symbol));
         Put ("expected: "); Put (Expected);
         Put ("computed: "); Put (Computed);
      end if;

      Check (Label, Computed, Expected);
   end Test_Goto_Transitions;

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
      use LR1;
      use LR1_AUnit;

      --  kernel:
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      --  FIXME: add nonterm that starts with BEGIN_ID after declarative_part

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 6, -- in grammar
         Dot  => 2,
         Next => null);

      Expected : Item_Set;

   begin
      if Test.Debug then
         Ada.Text_IO.Put ("kernel 1: "); Put (Kernel);
      end if;

      --  Expected goto_transitions on IS_ID:
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 6, -- in Grammar
         Dot  => 2,
         Next => null);

      Test_Goto_Transitions ("1", Kernel, IS_ID, Expected, Test.Debug);

      --  Expected goto_transitions on BEGIN_ID:
      --  BODY_ID <= IS_ID DECLARATIVE_PART_ID BEGIN_ID ^ SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 6, -- in Grammar
         Dot  => 4,
         Next => null);

      Test_Goto_Transitions ("2", Kernel, BEGIN_ID, Expected, Test.Debug);

      --  Expected goto_transitions on SEMICOLON_ID: none

      Expected :=
        (Set       => null,
         Goto_List => null,
         Index     => -1,
         Next      => null);

      Test_Goto_Transitions ("3", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_1;

   procedure Goto_Transitions_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR1;
      use LR1_AUnit;

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
         Index     => -1,
         Next      => null);

      Test_Goto_Transitions ("2", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_2;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_First'Access, "Test_First");
      Register_Routine (T, Goto_Transitions_1'Access, "Goto_Transitions_1");
      Register_Routine (T, Goto_Transitions_2'Access, "Goto_Transitions_2");
   end Register_Tests;

end Test_Empty_Productions;
