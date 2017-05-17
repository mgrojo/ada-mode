--  Abstract:
--
--  See spec
--
--  Copyright (C) 2017 Stephen Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The FastToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the FastToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with Ada.Text_IO;
with FastToken.Lexer;
with FastToken.Parser.LR1_Items;
with FastToken.Parser.LR;
with FastToken.Production;
with FastToken.Token;
with Gen_FastToken_AUnit;
package body Test_Follow is

   package Subprograms is
      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class);
   end Subprograms;
   package body Subprograms is

      type Token_ID is
        (
         --  Terminals
         Procedure_ID,
         Symbol_ID,
         Left_Paren_ID,
         Right_Paren_ID,
         EOF_ID,

         --  Nonterminal
         FastToken_Accept_ID,
         Declarations_ID,
         Declaration_ID,
         Subprogram_ID,
         Parameter_List_ID);

      package Token is new FastToken.Token (Token_ID, Token_ID'First, EOF_ID, Token_ID'Image);
      package Production is new FastToken.Production (Token);
      package Lexer_Root is new FastToken.Lexer (Token);
      package Parser_Root is new FastToken.Parser
        (Token_ID, Token_ID'First, EOF_ID, EOF_ID, FastToken_Accept_ID, Token_ID'Image, Ada.Text_IO.Put,
         Token, Lexer_Root);
      First_State_Index : constant := 1;
      package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Token.Get);
      package LR1_Items is new Parser_Root.LR1_Items
        (LR.Unknown_State_Index, LR.Unknown_State, Production);

      use all type Token.List.Instance;
      use all type Production.Right_Hand_Side;
      use all type Production.Instance;
      use all type Production.List.Instance;

      Null_Action : Token.Semantic_Action renames Token.Null_Action;

      --  This grammar has right recursion on Declarations_ID, and an
      --  empty production for Parameter_List_ID
      Grammar : constant Production.List.Instance :=
        FastToken_Accept_ID <= Declarations_ID & EOF_ID + Null_Action and                -- 1
        Declarations_ID     <= Declaration_ID + Null_Action and                          -- 2
        Declarations_ID     <= Declarations_ID & Declaration_ID + Null_Action and        -- 3
        Declaration_ID      <= Subprogram_ID + Null_Action and                           -- 4
        Subprogram_ID       <= Procedure_ID & Parameter_List_ID + Null_Action and        -- 5
        Parameter_List_ID   <= +Null_Action and                                          -- 6
        Parameter_List_ID   <= Left_Paren_ID & Symbol_ID & Right_Paren_ID + Null_Action; -- 7

      package FastToken_AUnit is new Gen_FastToken_AUnit
        (Token_ID, Token_ID'First, EOF_ID, Token, Production, Lexer_Root,
         Parser_Root, First_State_Index, LR, LR1_Items, Grammar);

      Has_Empty_Production : constant Token.Nonterminal_ID_Set := LR1_Items.Has_Empty_Production (Grammar);
      First                : constant Token.Nonterminal_Array_Token_Set := LR1_Items.First
        (Grammar, Has_Empty_Production, Trace => False);

      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class)
      is
         Test : Test_Case renames Test_Case (T);

         use Ada.Text_IO;
         use LR1_Items;
         use Token;

         Computed : constant Nonterminal_Array_Terminal_Set := Follow (Grammar, First, Has_Empty_Production);

         Expected : constant Nonterminal_Array_Terminal_Set :=
           (FastToken_Accept_ID => (others => False),
            Declarations_ID     => (EOF_ID | Procedure_ID => True, others => False),
            Declaration_ID      => (EOF_ID | Procedure_ID => True, others => False),
            Subprogram_ID       => (EOF_ID | Procedure_ID => True, others => False),
            Parameter_List_ID   => (EOF_ID | Procedure_ID => True, others => False));

      begin
         if Test.Debug then
            Put_Line ("Computed: ");
            Put (Computed);
            New_Line;
            Put_Line ("Expected:");
            Put (Expected);
         end if;
         FastToken_AUnit.Check ("1", Computed, Expected);
      end One;

   end Subprograms;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Subprograms.One'Access, "Subprograms.One");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_follow.adb");
   end Name;

end Test_Follow;
