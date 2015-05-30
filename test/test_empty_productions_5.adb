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

with Ada.Text_IO;
with FastToken.Lexer;
with FastToken.Parser.LALR.Generator;
with FastToken.Production;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Test_Empty_Productions_5 is

   --  A grammar with a null production following a nonterm (from ../wisi/test/empty_production_5.wy)

   type Token_ID is
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

   package Token_Pkg is new FastToken.Token (Token_ID, ACCEPT_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   package LALR is new Parser_Root.LALR (First_State_Index => 1, Nonterminal => Nonterminal);
   package LALR_Generator is new LALR.Generator (Token_ID'Width, Production);

   --  Allow infix operators for building productions
   use type Token_Pkg.List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production.List.Instance;

   function "+" (Item : in Token_ID) return Token_Pkg.Instance'Class renames Token_Pkg."+";

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     Nonterminal.Get (opentoken_accept_ID) <= (+compilation_unit_ID) & (+EOF_ID) + Self -- 1
     and
     Nonterminal.Get (compilation_unit_ID) <= (+accept_statement_ID) & (+accept_statement_ID) + Self -- 2
     and
     Nonterminal.Get (accept_statement_ID) <= (+ACCEPT_ID) & (+name_ID) & (+parameter_profile_ID) &
     (+SEMICOLON_ID) + Self -- 3
     and
     Nonterminal.Get (name_ID) <= (+IDENTIFIER_ID) + Self -- 4
     and
     Nonterminal.Get (parameter_profile_ID) <= +Self -- 5; empty
     and
     Nonterminal.Get (parameter_profile_ID) <= (+LEFT_PAREN_ID) & (+IDENTIFIER_ID) & (+LEFT_PAREN_ID) + Self -- 6
     ;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, ACCEPT_ID, EOF_ID, Token_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, 1, LALR, LALR_Generator, Grammar);

   Has_Empty_Production : constant LALR_Generator.LRk.Nonterminal_ID_Set :=
     LALR_Generator.LRk.Has_Empty_Production (Grammar);

   First : constant LALR_Generator.LRk.Derivation_Matrix :=
     LALR_Generator.LRk.First_Derivations (Grammar, Has_Empty_Production, Trace => False);

   ----------
   --  Test procedures

   procedure Test_Lookahead_Closure (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LRk;
      use FastToken_AUnit;

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 3, -- in grammar
         Dot  => 2,
         Next => null);

      Closure : constant Item_Set := LALR_Generator.LRk.Lookahead_Closure
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
         State     => LALR.Unknown_State,
         Next      => null);

      if Test.Debug then
         Ada.Text_IO.Put_Line ("Expected:");
         LALR_Generator.LRk.Put (Expected);
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
