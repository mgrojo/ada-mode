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
with Gen_FastToken_AUnit;
with FastToken.Lexer;
with FastToken.Production;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Token.Nonterminal;
package body Test_Empty_Productions_8 is

   --  A grammar with two consecutive possibly empty productions,
   --  confused with a similar production (from Emacs Ada mode
   --  test/wisi/empty_production_8.wy)

   type Token_ID is
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

   package Token_Pkg is new FastToken.Token (Token_ID, COLON_EQUAL_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package LALR_Generator is new LR.LALR_Generator (EOF_ID, Production);

   --  Allow infix operators for building productions
   use type Token_Pkg.List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production.List.Instance;

   function "+" (Item : in Token_ID) return Token_Pkg.Instance'Class renames Token_Pkg."+";

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     Nonterminal.Get (opentoken_accept_ID) <= (+object_declaration_list_ID) & (+EOF_ID) + Self -- 1
     and
     Nonterminal.Get (object_declaration_list_ID) <= (+object_declaration_ID) + Self -- 2
     and
     Nonterminal.Get (object_declaration_list_ID) <= (+object_declaration_list_ID) & (+object_declaration_ID) +
     Self -- 3
     and
     Nonterminal.Get (object_declaration_ID) <= (+IDENTIFIER_ID) & (+aliased_opt_ID) & (+constant_opt_ID) &
     (+SEMICOLON_ID) + Self -- 4
     and
     Nonterminal.Get (object_declaration_ID) <= (+IDENTIFIER_ID) & (+constant_opt_ID) & (+COLON_EQUAL_ID) &
     (+SEMICOLON_ID) + Self -- 5
     and
     Nonterminal.Get (aliased_opt_ID) <= +Self -- 6
     and
     Nonterminal.Get (aliased_opt_ID) <= (+ALIASED_ID) + Self -- 7
     and
     Nonterminal.Get (constant_opt_ID) <= +Self -- 8
     and
     Nonterminal.Get (constant_opt_ID) <= (+CONSTANT_ID) + Self -- 9
     ;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, COLON_EQUAL_ID, EOF_ID, Token_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, First_State_Index, LR, LALR_Generator.LR1_Items, Grammar);

   Has_Empty_Production : constant LALR_Generator.LR1_Items.Nonterminal_ID_Set :=
     LALR_Generator.LR1_Items.Has_Empty_Production (Grammar);

   First : constant LALR_Generator.LR1_Items.Derivation_Matrix := LALR_Generator.LR1_Items.First
     (Grammar, Has_Empty_Production, Trace => False);

   ----------
   --  Test procedures

   procedure Kernels_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LALR_Generator.LR1_Items;

      Kernels : constant Item_Set_List := LALR_Generator.LR1_Items.Kernels
        (Grammar, First, EOF_ID, Trace => Test.Debug, First_State_Index => LR.Unknown_State_Index (First_State_Index));

      procedure Check_Kernel
        (State : in LR.State_Index;
         Prod  : in Integer;
         Dot   : in Integer)
      is
         use FastToken_AUnit;
         Computed : constant Item_Set_Ptr := Find (State, Kernels);

         Expected : constant Item_Set_Ptr := new Item_Set'
           (Set             => Get_Item_Node
              (Prod         => Prod,
               Lookaheads   => null,
               Dot          => Dot,
               State        => State),
            Goto_List       => null,
            State           => State,
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

         Check (LR.State_Index'Image (State), Computed.all, Expected.all);

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
