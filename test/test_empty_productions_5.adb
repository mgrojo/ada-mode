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

with Ada.Text_IO;
with Gen_OpenToken_AUnit;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Empty_Productions_5 is

   --  A grammar with a null production following a nonterm (from ../wisi/test/empty_production_5.wy)

   type Token_IDs is
     (
      --  non-reporting
      Whitespace_ID,

      --  terminals
      ACCEPT_ID,
      IDENTIFIER_ID,
      LEFT_PAREN_ID,
      RIGHT_PAREN_ID,
      SEMICOLON_ID,
      EOF_ID,

      --  non-terminals
      opentoken_accept_ID,
      compilation_unit_ID,
      accept_statement_ID,
      name_ID,
      parameter_profile_ID);

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => ACCEPT_ID,
      Last_Terminal  => EOF_ID);
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALR is new Parsers.LALR (First_State_Index => 1);

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
     Nonterminals.Get (compilation_unit_ID) <= (+accept_statement_ID) & (+accept_statement_ID) + Self -- 2
     and
     Nonterminals.Get (accept_statement_ID) <= (+ACCEPT_ID) & (+name_ID) & (+parameter_profile_ID) &
     (+SEMICOLON_ID) + Self -- 3
     and
     Nonterminals.Get (name_ID) <= (+IDENTIFIER_ID) + Self -- 4
     and
     Nonterminals.Get (parameter_profile_ID) <= +Self -- 5; empty
     and
     Nonterminals.Get (parameter_profile_ID) <= (+LEFT_PAREN_ID) & (+IDENTIFIER_ID) & (+LEFT_PAREN_ID) + Self -- 6
     ;

   package OpenToken_AUnit is new Gen_OpenToken_AUnit
     (Token_IDs, Tokens_Pkg, Token_Lists, Nonterminals, Productions, Production_Lists, ACCEPT_ID, EOF_ID,
      Analyzers, Parsers, 1, LALR, Grammar);

   Has_Empty_Production : constant LALR.LRk.Nonterminal_ID_Set := LALR.LRk.Has_Empty_Production (Grammar);

   First : constant LALR.LRk.Derivation_Matrix :=
     LALR.LRk.First_Derivations (Grammar, Has_Empty_Production, Trace => False);

   ----------
   --  Test procedures

   procedure Test_Lookahead_Closure (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR.LRk;
      use OpenToken_AUnit;

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 3, -- in grammar
         Dot  => 2,
         Next => null);

      Closure : constant Item_Set := LALR.LRk.Lookahead_Closure
        (Kernel, Has_Empty_Production, First, Grammar, Trace => Test.Debug);

      Expected_Set : Item_Ptr;
      Expected     : Item_Set;

   begin
      --  Kernel 2:
      --
      --  ACCEPT_STATEMENT_ID <= ACCEPT_ID ^ NAME_ID PARAMETER_PROFILE_ID SEMICOLON_ID

      --  Expected lookahead closure:
      --
      --  NAME_ID <= ^ IDENTIFIER_ID, SEMICOLON_ID/LEFT_PAREN_ID
      --  ACCEPT_STATEMENT_ID <= ACCEPT_ID ^ NAME_ID PARAMETER_PROFILE_ID SEMICOLON_ID, <no lookaheads>

      Expected_Set := Get_Item_Node
        (Prod       => 3, -- in grammar
         Lookaheads => null,
         Dot        => 2,
         Next       => null);

      Expected_Set := Get_Item_Node
        (Prod       => 4, -- in grammar
         Lookaheads => +(SEMICOLON_ID, LEFT_PAREN_ID),
         Dot        => 1,
         Next       => Expected_Set);

      Expected :=
        (Set       => Expected_Set,
         Goto_List => null,
         Index     => -1,
         Next      => null);

      if Test.Debug then
         Ada.Text_IO.Put_Line ("Expected:");
         LALR.LRk.Put (Expected);
         Ada.Text_IO.New_Line;
      end if;

      Check ("1", Closure, Expected);
   end Test_Lookahead_Closure;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_5.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Lookahead_Closure'Access, "Test_Lookahead_Closure");
   end Register_Tests;

end Test_Empty_Productions_5;
