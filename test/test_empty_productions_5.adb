--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017 Stephen Leake.  All Rights Reserved.
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
with FastToken.Parser.LR;
with FastToken.Parser.LR1_Items;
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
      fasttoken_accept_ID,
      compilation_unit_ID,
      accept_statement_ID,
      name_ID,
      parameter_profile_ID);

   package Token_Pkg is new FastToken.Token (Token_ID, ACCEPT_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, EOF_ID, fasttoken_accept_ID, Lexer_Root);
   First_State_Index : constant := 1;
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);

   --  Allow infix operators for building productions
   use type Token_Pkg.List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production.List.Instance;

   function "+" (Item : in Token_ID) return Token_Pkg.Instance'Class renames Token_Pkg."+";

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     Nonterminal.Get (fasttoken_accept_ID) <= (+compilation_unit_ID) & (+EOF_ID) + Self -- 1
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
      Lexer_Root, Parser_Root, 1, LR, LR1_Items, Grammar);

   Has_Empty_Production : constant LR1_Items.Nonterminal_ID_Set :=
     LR1_Items.Has_Empty_Production (Grammar);

   First : constant LR1_Items.Derivation_Matrix :=
     LR1_Items.First (Grammar, Has_Empty_Production, Trace => False);

   ----------
   --  Test procedures

   procedure Test_Closure (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR1_Items;
      use FastToken_AUnit;

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 3, -- in grammar
         Dot  => 2,
         Next => null);

      Closure : constant Item_Set := LR1_Items.Closure
        (Kernel, Has_Empty_Production, First, Grammar, Trace => Test.Debug);
   begin
      --  Kernel 2:
      --
      --  ACCEPT_STATEMENT_ID <= ACCEPT_ID ^ NAME_ID PARAMETER_PROFILE_ID SEMICOLON_ID

      --  Expected lookahead closure:
      --
      --  NAME_ID <= ^ IDENTIFIER_ID, SEMICOLON_ID/LEFT_PAREN_ID
      --  ACCEPT_STATEMENT_ID <= ACCEPT_ID ^ NAME_ID PARAMETER_PROFILE_ID SEMICOLON_ID, <no lookaheads>

      Check
        ("1", Closure,
         +(Get_Item (3, 2, Null_Lookaheads) &
             Get_Item (4, 1, +(SEMICOLON_ID, LEFT_PAREN_ID))));
   end Test_Closure;

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
      Register_Routine (T, Test_Closure'Access, "Test_Closure");
   end Register_Tests;

end Test_Empty_Productions_5;
