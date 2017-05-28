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

with AUnit.Assertions;
with Ada.Text_IO;
with FastToken.Lexer;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.LR1_Generator;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Token;
with FastToken.Token_Plain;
with Gen_FastToken_AUnit;
package body Compare_Goto_Transitions is

   ----------
   --  Test procedures

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

      package Token_Pkg is new FastToken.Token (Token_ID, Token_ID'First, EOF_ID, Token_ID'Image);
      package Lexer_Root is new FastToken.Lexer (Token_ID);
      package Token_Aug is new FastToken.Token_Plain (Token_Pkg, Lexer_Root);
      package Production is new FastToken.Production (Token_Pkg, Token_Aug.Semantic_Action, Token_Aug.Null_Action);
      package Parser_Root is new FastToken.Parser
        (Token_ID, Token_ID'First, EOF_ID, EOF_ID, FastToken_Accept_ID, Token_ID'Image, Ada.Text_IO.Put,
         Token_Pkg, Lexer_Root);
      First_State_Index : constant := 1;
      package LR is new Parser_Root.LR
        (First_State_Index, Token_ID'Width, Token_Aug.Semantic_Action, Token_Aug.Null_Action,
         Token_Aug.State_Type, Token_Aug.Input_Token);
      package LR1_Items is new Parser_Root.LR1_Items
        (LR.Unknown_State_Index, LR.Unknown_State, Token_Aug.Semantic_Action, Token_Aug.Null_Action, Production);
      package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
      package LALR_Generator is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);
      package LR1_Generator is new LR.LR1_Generator (Production, LR1_Items, Generator_Utils);

      use all type Token_Pkg.List.Instance;
      use all type Production.Right_Hand_Side;
      use all type Production.Instance;
      use all type Production.List.Instance;

      Null_Action : Token_Aug.Semantic_Action renames Token_Aug.Null_Action;

      --  This grammar has an empty production (number 6); test that
      --  Closure and Goto_Transitions handle it properly.
      Grammar : constant Production.List.Instance :=
        FastToken_Accept_ID <= Declarations_ID & EOF_ID + Null_Action and                -- 1
        Declarations_ID     <= Declaration_ID + Null_Action and                          -- 2
        Declarations_ID     <= Declarations_ID & Declaration_ID + Null_Action and        -- 3
        Declaration_ID      <= Subprogram_ID + Null_Action and                           -- 4
        Subprogram_ID       <= Procedure_ID & Parameter_List_ID + Null_Action and        -- 5
        Parameter_List_ID   <= +Null_Action and                                          -- 6
        Parameter_List_ID   <= Left_Paren_ID & Symbol_ID & Right_Paren_ID + Null_Action; -- 7

      package FastToken_AUnit is new Gen_FastToken_AUnit
        (Token_ID, Token_ID'First, EOF_ID, Token_Pkg, Token_Aug.Semantic_Action, Token_Aug.Null_Action, Production,
         Lexer_Root, Parser_Root, LR.Unknown_State_Index, LR.Unknown_State, LR1_Items,
         Grammar);

      Has_Empty_Production : constant Token_Pkg.Nonterminal_ID_Set := LR1_Items.Has_Empty_Production (Grammar);
      First                : constant Token_Pkg.Nonterminal_Array_Token_Set := LR1_Items.First
        (Grammar, Has_Empty_Production, Trace => False);

      procedure Compare (Prod : in Integer; Symbol : in Token_Pkg.Terminal_ID; Trace : in Boolean)
      is
         use Ada.Text_IO;
         use LR1_Items;

         Set : constant Item_Set := Closure
           ((Set      => FastToken_AUnit.Get_Item_Node (Prod, 1, +Symbol),
             Goto_List => null,
             State     => LR.Unknown_State,
             Next      => null),
           Has_Empty_Production, First, Grammar,
           Trace      => False);

         LR1           : Item_Set;
         LR1_Filtered  : Item_Set;
         LALR          : Item_Set;
         Mismatch      : Boolean := False;
      begin
         for ID in Token_ID loop
            declare
               Label : constant String :=
                 Integer'Image (Prod) & "." &
                 Token_ID'Image (Symbol) & "." &
                 Token_ID'Image (ID);
            begin
               LR1 := LR1_Generator.LR1_Goto_Transitions
                    (Set, ID, Has_Empty_Production, First, Grammar, Trace => False);
               LR1_Filtered := Filter (LR1, In_Kernel'Access);
               Free (LR1);

               LALR := LALR_Generator.LALR_Goto_Transitions (Set, ID, First, Grammar, Trace => False);

               FastToken_AUnit.Check (Label, LR1_Filtered, LALR, Match_Lookaheads => False);
               Free (LR1_Filtered);
               Free (LALR);
            exception
            when AUnit.Assertions.Assertion_Error =>
               Mismatch := True;
               if Trace then
                  Put_Line (Label & " mismatch");
                  Put_Line ("  lr1:");
                  Put (LR1_Filtered, Show_Goto_List => True);
                  New_Line;
                  Put_Line ("  lalr:");
                  Put (LALR, Show_Goto_List => True);
                  New_Line;
                  Free (LR1_Filtered);
                  Free (LALR);
               else
                  Free (LR1_Filtered);
                  Free (LALR);
                  raise;
               end if;
            end;
         end loop;
         if Trace and Mismatch then
            Put_Line (Integer'Image (Prod) & "." & Token_ID'Image (Symbol) & ".closure");
            Put (Set);
            New_Line;
         end if;

      end Compare;

      procedure One (T : in out AUnit.Test_Cases.Test_Case'Class)
      is
         Test : Test_Case renames Test_Case (T);
      begin
         for Prod in 1 .. 7 loop
            for Symbol in Token_Pkg.Terminal_ID loop
               Compare (Prod, Symbol, Test.Debug);
            end loop;
         end loop;
      end One;

   end Subprograms;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Subprograms.One'Access, "Subprograms");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/compare_goto_transitions.adb");
   end Name;

end Compare_Goto_Transitions;
