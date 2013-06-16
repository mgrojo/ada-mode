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
package body Test_Empty_Productions_8 is

   --  A grammar with two consecutive possibly empty productions,
   --  confused with a similar production (from Emacs Ada mode
   --  test/wisi/empty_production_8.wy)

   type Token_IDs is
     (
      --  non-reporting
      Whitespace_ID,
      COMMENT_ID,

      --  terminals
      COLON_EQUAL_ID,
      SEMICOLON_ID,
      IDENTIFIER_ID,
      ALIASED_ID,
      CONSTANT_ID,
      EOF_ID,

      --  non-terminals
      object_declaration_list_ID,
      object_declaration_ID,
      aliased_opt_ID,
      constant_opt_ID,
      opentoken_accept_ID);

   First_State_Index : constant Integer := 0;

   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => COLON_EQUAL_ID,
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
     Nonterminals.Get (object_declaration_ID) <= (+IDENTIFIER_ID) & (+constant_opt_ID) & (+COLON_EQUAL_ID) &
     (+SEMICOLON_ID) + Self -- 5
     and
     Nonterminals.Get (aliased_opt_ID) <= +Self -- 6
     and
     Nonterminals.Get (aliased_opt_ID) <= (+ALIASED_ID) + Self -- 7
     and
     Nonterminals.Get (constant_opt_ID) <= +Self -- 8
     and
     Nonterminals.Get (constant_opt_ID) <= (+CONSTANT_ID) + Self -- 9
     ;

   package OpenToken_AUnit is new Gen_OpenToken_AUnit
     (Token_IDs, Tokens_Pkg, Token_Lists, Nonterminals, Productions, Production_Lists, COLON_EQUAL_ID, EOF_ID,
      Analyzers, Parsers, First_State_Index, LALR, Grammar);

   Has_Empty_Production : constant LALR.LRk.Nonterminal_ID_Set := LALR.LRk.Has_Empty_Production (Grammar);

   First : constant LALR.LRk.Derivation_Matrix := LALR.LRk.First_Derivations
     (Grammar, Has_Empty_Production, Trace => False);

   ----------
   --  Test procedures

   procedure Kernels_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR.LRk;

      Kernels : constant Item_Set_List := LR0_Kernels
        (Grammar, First, Trace => Test.Debug, First_State_Index => First_State_Index);

      procedure Check_Kernel
        (State : in Integer;
         Prod  : in Integer;
         Dot   : in Integer)
      is
         use OpenToken_AUnit;
         Computed : constant Item_Set_Ptr := Find (State, Kernels);

         Expected : constant Item_Set_Ptr := new Item_Set'
           (Set             => Get_Item_Node
              (Prod         => Prod,
               Lookaheads   => null,
               Dot          => Dot,
               Index        => State),
            Goto_List       => null,
            Index           => State,
            Next            => null);
      begin
         Computed.Next := null;
         Computed.Goto_List := null;

         if Test.Debug then
            Ada.Text_IO.Put_Line ("Computed:");
            Put (Computed.all);
            Ada.Text_IO.Put_Line ("Expected:");
            Put (Expected.all);
         end if;

         Check (Integer'Image (State), Computed.all, Expected.all);

      end Check_Kernel;

   begin
      if Test.Debug then
         Put (Kernels);
      end if;

      --  In a previous version, kernel 9 had dot both before and
      --  after CONSTANT_ID; now that is split into two kernels.

      Check_Kernel (8, 4, 3);
      Check_Kernel (9, 5, 3);
   end Kernels_1;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_8.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Kernels_1'Access, "Kernels_1");
   end Register_Tests;

end Test_Empty_Productions_8;
--  Local Variables:
--  ada-case-strict: nil
--  End:
