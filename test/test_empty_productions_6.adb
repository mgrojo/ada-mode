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
package body Test_Empty_Productions_6 is

   --  A grammar with a null production in a nonterm in a list (from
   --  Emacs Ada mode test/wisi/empty_production_6.wy); same conflict
   --  is present in two states.

   type Token_IDs is
     (
      --  non-reporting
      Whitespace_ID,
      COMMENT_ID,

      --  terminals
      COLON_ID,
      COLON_EQUAL_ID,
      SEMICOLON_ID,
      IDENTIFIER_ID,
      BEGIN_ID,
      END_ID,
      EOF_ID,

      --  non-terminals
      compilation_unit_ID,
      statement_ID,
      sequence_of_statements_ID,
      label_opt_ID,
      opentoken_accept_ID);

   First_State_Index : constant Integer := 0;

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, COLON_ID, EOF_ID, Token_IDs'Image);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Analyzers is new Tokens_Pkg.Analyzer;
   package Parsers is new Productions.Parser (Analyzers);
   package LALRs is new Parsers.LALR (First_State_Index);
   package LALR_Generators is new LALRs.Generator (Token_IDs'Width, Production_Lists);

   --  Allow infix operators for building productions
   use type Token_Lists.Instance;
   use type Productions.Right_Hand_Side;
   use type Productions.Instance;
   use type Production_Lists.Instance;

   function "+" (Item : in Token_IDs) return Tokens_Pkg.Instance'Class renames Tokens_Pkg."+";

   Self : Nonterminals.Synthesize renames Nonterminals.Synthesize_Self;

   Grammar : constant Production_Lists.Instance :=
     Nonterminals.Get (opentoken_accept_ID) <= (+compilation_unit_ID) & (+EOF_ID) -- 1
     and
     Nonterminals.Get (compilation_unit_ID) <= (+BEGIN_ID) & (+sequence_of_statements_ID) & (+END_ID) &
     (+SEMICOLON_ID) + Self -- 2
     and
     Nonterminals.Get (statement_ID) <= (+label_opt_ID) & (+IDENTIFIER_ID) & (+COLON_EQUAL_ID) & (+IDENTIFIER_ID) &
     (+SEMICOLON_ID) + Self -- 3
     and
     Nonterminals.Get (sequence_of_statements_ID) <= (+statement_ID) + Self -- 4
     and
     Nonterminals.Get (sequence_of_statements_ID) <= (+sequence_of_statements_ID) & (+statement_ID) + Self -- 5
     and
     Nonterminals.Get (label_opt_ID) <= +Self -- 6
     and
     Nonterminals.Get (label_opt_ID) <= (+IDENTIFIER_ID) & (+COLON_ID) + Self -- 7
     ;

   package OpenToken_AUnit is new Gen_OpenToken_AUnit
     (Token_IDs, COLON_ID, EOF_ID, Tokens_Pkg, Token_Lists, Nonterminals, Productions, Production_Lists,
      Analyzers, Parsers, First_State_Index, LALRs, LALR_Generators, Grammar);

   Has_Empty_Production : constant LALR_Generators.LRk.Nonterminal_ID_Set :=
     LALR_Generators.LRk.Has_Empty_Production (Grammar);

   First : constant LALR_Generators.LRk.Derivation_Matrix := LALR_Generators.LRk.First_Derivations
     (Grammar, Has_Empty_Production, Trace => False);

   Accept_Index : constant := 3;

   procedure Test_Actions
     (Label    : in String;
      Kernels  : in LALR_Generators.LRk.Item_Set_List;
      State    : in LALRs.Unknown_State_Index;
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
         LALR_Generators.Put (Table (Kernel.State));
         Ada.Text_IO.Put_Line ("Expected:");
         LALR_Generators.Put (Expected);
      end if;

      Check (Label, Table (Kernel.State), Expected);
   end Test_Actions;

   ----------
   --  Test procedures

   procedure Actions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALRs;
      use LALR_Generators.LRk;
      use OpenToken_AUnit;

      Kernels : constant Item_Set_List := LR0_Kernels
        (Grammar, First,
         Trace             => False,
         First_State_Index => LALRs.State_Index (First_State_Index));

      Expected : Parse_State;
      Conflict : Parse_Action_Node_Ptr;

      Conflicts : LALRs.Conflict_Lists.List;
      Parser    : LALRs.Parse_Table_Ptr;
   begin
      if Test.Debug then
         LALR_Generators.LRk.Put (Kernels);
      end if;

      --  kernel 1:
      --
      --  compilation_unit <= BEGIN ^ sequence_of_statements END SEMICOLON
      --
      --  Expected actions:
      --  IDENTIFIER_ID => shift and goto 7
      --  IDENTIFIER_ID => reduce 0 tokens to label_opt_id
      --  default  => ERROR

      --  Expected reduction gotos:
      --  label_opt_ID => Set 7
      --  statement_ID => Set 5
      --  sequence_of_statements_ID => Set 6

      Expected.Action_List := new Action_Node'
        (Symbol  => Tokens_Pkg.Terminal_ID'Last, -- ignored, since this is the last action
         Action  => new Parse_Action_Node'
           (Item => (Verb => Error),
            Next => null),
         Next  => null);

      Conflict := new Parse_Action_Node'
        (Item           =>
           (Verb        => Reduce,
            LHS         => Productions.LHS (Get_Production (6)),
            Action      => null,
            Index       => 0,
            Token_Count => 0),
         Next           => null);

      Conflict := new Parse_Action_Node'
        (Item     =>
           (Verb  => Shift,
            State => 4),
         Next     => Conflict);

      Expected.Action_List := new Action_Node'
        (Symbol      => IDENTIFIER_ID,
         Action      => Conflict,
         Next        => Expected.Action_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => label_opt_ID,
         State  => 7,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => sequence_of_statements_ID,
         State  => 6,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => statement_ID,
         State  => 5,
         Next   => Expected.Goto_List);

      Test_Actions ("1", Kernels, 1, Expected, Test.Debug);

      --  Previous versions of OpenToken did not allow the same
      --  conflict to occur twice, so the action would be lost in
      --  Generate.
      Conflicts.Append
        ((Action_A    => Shift,
          LHS_A       => label_opt_ID,
          Action_B    => Reduce,
          LHS_B       => label_opt_ID,
          State_Index => -1,
          On          => IDENTIFIER_ID));

      Parser := LALR_Generators.Generate
        (Grammar,
         Conflicts,
         Trace                    => Test.Debug,
         Put_Parse_Table          => Test.Debug,
         Ignore_Unused_Tokens     => Test.Debug,
         Ignore_Unknown_Conflicts => Test.Debug);

      Check ("2", Parser (1), Expected);
   end Actions_1;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_6.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Actions_1'Access, "Actions_1");
   end Register_Tests;

end Test_Empty_Productions_6;
