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
with OpenToken.Production.List;
with OpenToken.Production.Parser.LALR.Generator;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_Empty_Productions_4 is

   --  A grammar with a null production in the first two nonterms in a
   --  production (from ../wisi/test/empty_production_4.wy)

   type Token_IDs is
     (
      --  non-reporting
      Whitespace_ID,

      --  terminals
      IDENTIFIER_ID,
      OVERRIDING_ID,
      PROCEDURE_ID,
      SEMICOLON_ID,
      EOF_ID,

      --  non-terminals
      opentoken_accept_ID,
      compilation_unit_ID,
      subprogram_declaration_ID,
      overriding_indicator_ID);

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => IDENTIFIER_ID,
      Last_Terminal  => EOF_ID);
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);
   package LALRs is new Parsers.LALR (First_State_Index => 1);
   package LALR_Generators is new LALRs.Generator;

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
     Nonterminals.Get (compilation_unit_ID) <= (+subprogram_declaration_ID) & (+subprogram_declaration_ID) + Self -- 2
     and
     Nonterminals.Get (subprogram_declaration_ID) <= (+overriding_indicator_ID) & (+PROCEDURE_ID) &
     (+IDENTIFIER_ID) & (+SEMICOLON_ID) + Self -- 3
     and
     Nonterminals.Get (overriding_indicator_ID) <= (+OVERRIDING_ID) + Self -- 4
     and
     Nonterminals.Get (overriding_indicator_ID) <= +Self -- 5; empty
     ;

   Has_Empty_Production : constant LALR_Generators.LRk.Nonterminal_ID_Set :=
     LALR_Generators.LRk.Has_Empty_Production (Grammar);

   First : constant LALR_Generators.LRk.Derivation_Matrix :=
     LALR_Generators.LRk.First_Derivations (Grammar, Has_Empty_Production, Trace => False);

   ----------
   --  Test procedures

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Check;
   begin
      Check ("1", First (subprogram_declaration_ID)(PROCEDURE_ID), True);
   end Test_First;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_4.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_First'Access, "Test_First");
   end Register_Tests;

end Test_Empty_Productions_4;
