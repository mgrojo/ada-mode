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
with OpenToken.Production.Parser.LALR;
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

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => COLON_ID,
      Last_Terminal  => EOF_ID);
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALR is new Parsers.LALR (First_State_Index);

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
     (Token_IDs, Tokens_Pkg, Token_Lists, Nonterminals, Productions, Production_Lists, COLON_ID, EOF_ID,
      Analyzers, Parsers, First_State_Index, LALR, Grammar);

   Has_Empty_Production : constant LALR.LRk.Nonterminal_ID_Set := LALR.LRk.Has_Empty_Production (Grammar);

   First : constant LALR.LRk.Derivation_Matrix := LALR.LRk.First_Derivations
     (Grammar, Has_Empty_Production, Trace => False);

   Accept_Index : constant := 3;

   procedure Test_Actions
     (Label    : in String;
      Kernels  : in LALR.LRk.Item_Set_List;
      State    : in Integer;
      Expected : in LALR.Parse_State;
      Debug    : in Boolean)
   is
      use OpenToken_AUnit;
      Kernel    : constant LALR.LRk.Item_Set_Ptr := LALR.LRk.Find (State, Kernels);
      Conflicts : LALR.Conflict_Lists.List;
      Table     : LALR.Parse_Table (1 .. LALR.State_Index (Kernels.Size));
   begin
      LALR.Add_Actions
        (Kernel, Accept_Index, Grammar, Has_Empty_Production, First, Conflicts, Table, Trace => Debug);

      if Debug then
         LALR.Put (Table (LALR.State_Index (Kernel.Index)));
         Ada.Text_IO.Put_Line ("Expected:");
         LALR.Put (Expected);
      end if;

      Check (Label, Table (LALR.State_Index (Kernel.Index)), Expected);
   end Test_Actions;

   ----------
   --  Test procedures

   procedure Actions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR;
      use LALR.LRk;
      use OpenToken_AUnit;

      Kernels : constant Item_Set_List := LR0_Kernels
        (Grammar, First, Trace => False, First_State_Index => First_State_Index);

      Expected : Parse_State;
      Conflict : Parse_Action_Node_Ptr;

      Conflicts : LALR.Conflict_Lists.List;
      Parser    : LALR.Instance;
   begin
      if Test.Debug then
         LALR.LRk.Put (Kernels);
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
        (Symbol  => Analyzers.Terminal_ID'Last, -- ignored, since this is the last action
         Action  => new Parse_Action_Node'
           (Item => (Verb => Error),
            Next => null),
         Next  => null);

      Conflict := new Parse_Action_Node'
        (Item          =>
           (Verb       => Reduce,
            Production => Get_Production (6),
            Length     => 0),
         Next          => null);

      Conflict := new Parse_Action_Node'
        (Item     =>
           (Verb  => Shift,
            State => 4),
         Next     => Conflict);

      Expected.Action_List := new Action_Node'
        (Symbol      => IDENTIFIER_ID,
         Action      => Conflict,
         Next        => Expected.Action_List);

      Expected.Reduction_List := new Reduction_Node'
        (Symbol => label_opt_ID,
         State  => 7,
         Next   => Expected.Reduction_List);

      Expected.Reduction_List := new Reduction_Node'
        (Symbol => sequence_of_statements_ID,
         State  => 6,
         Next   => Expected.Reduction_List);

      Expected.Reduction_List := new Reduction_Node'
        (Symbol => statement_ID,
         State  => 5,
         Next   => Expected.Reduction_List);

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

      Parser := LALR.Generate
        (Grammar,
         Analyzers.Null_Analyzer,
         Conflicts,
         Trace                    => Test.Debug,
         Put_Grammar              => Test.Debug,
         Ignore_Unused_Tokens     => Test.Debug,
         Ignore_Unknown_Conflicts => Test.Debug);

      Check ("2", Parser.Table (1), Expected);
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
