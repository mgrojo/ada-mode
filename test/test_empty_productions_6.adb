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

with Ada.Text_IO;
with FastToken.Lexer;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Test_Empty_Productions_6 is

   --  A grammar with a null production in a nonterm in a list (from
   --  Emacs Ada mode test/wisi/empty_production_6.wy); same conflict
   --  is present in two states.

   type Token_ID is
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

   package Token_Pkg is new FastToken.Token (Token_ID, COLON_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, EOF_ID, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package Generators is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

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
     Nonterminal.Get (compilation_unit_ID) <= (+BEGIN_ID) & (+sequence_of_statements_ID) & (+END_ID) &
     (+SEMICOLON_ID) + Self -- 2
     and
     Nonterminal.Get (statement_ID) <= (+label_opt_ID) & (+IDENTIFIER_ID) & (+COLON_EQUAL_ID) & (+IDENTIFIER_ID) &
     (+SEMICOLON_ID) + Self -- 3
     and
     Nonterminal.Get (sequence_of_statements_ID) <= (+statement_ID) + Self -- 4
     and
     Nonterminal.Get (sequence_of_statements_ID) <= (+sequence_of_statements_ID) & (+statement_ID) + Self -- 5
     and
     Nonterminal.Get (label_opt_ID) <= +Self -- 6
     and
     Nonterminal.Get (label_opt_ID) <= (+IDENTIFIER_ID) & (+COLON_ID) + Self -- 7
     ;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, COLON_ID, EOF_ID, Token_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, First_State_Index, LR, LR1_Items, Grammar);

   Has_Empty_Production : constant LR1_Items.Nonterminal_ID_Set :=
     LR1_Items.Has_Empty_Production (Grammar);

   First : constant LR1_Items.Derivation_Matrix := LR1_Items.First
     (Grammar, Has_Empty_Production, Trace => False);

   procedure Test_Actions
     (Label    : in String;
      Kernels  : in LR1_Items.Item_Set_List;
      State    : in LR.Unknown_State_Index;
      Expected : in LR.Parse_State;
      Debug    : in Boolean)
   is
      use FastToken_AUnit;
      Kernel    : constant LR1_Items.Item_Set_Ptr := LR1_Items.Find (State, Kernels);
      Closure   : LR1_Items.Item_Set              := LR1_Items.Closure
        (Kernel.all, Has_Empty_Production, First, Grammar, Match_Lookaheads => False, Trace => False);
      Conflicts : Generator_Utils.Conflict_Lists.List;
      Table     : LR.Parse_Table (1 .. LR.State_Index (Kernels.Size));
   begin
      Generator_Utils.Add_Actions
        (Closure, Table, Has_Empty_Production, Conflicts, Trace => Debug);

      LR1_Items.Free (Closure);

      if Debug then
         LR.Put (Table (Kernel.State));
         Ada.Text_IO.Put_Line ("Expected:");
         LR.Put (Expected);
      end if;

      Check (Label, Table (Kernel.State), Expected);
   end Test_Actions;

   ----------
   --  Test procedures

   procedure Actions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR;
      use LR1_Items;
      use FastToken_AUnit;

      Kernels : constant Item_Set_List := Generators.LALR_Kernels
        (Grammar, First,
         Trace             => False,
         First_State_Index => LR.State_Index (First_State_Index));

      Expected : Parse_State;
      Conflict : Parse_Action_Node_Ptr;

      Conflicts : Generator_Utils.Conflict_Lists.List;
      Parser    : LR.Parse_Table_Ptr;
   begin
      if Test.Debug then
         Put (Kernels);
      end if;

      --  State 1:
      --    COMPILATION_UNIT_ID <= BEGIN_ID ^ SEQUENCE_OF_STATEMENTS_ID END_ID SEMICOLON_ID
      --
      --  Expected actions:
      --  IDENTIFIER_ID => shift and goto 3
      --  IDENTIFIER_ID => reduce 0 tokens to label_opt_id
      --  default  => ERROR

      --  Expected reduction gotos:
      --  label_opt_ID              => State 6
      --  statement_ID              => State 4
      --  sequence_of_statements_ID => State 5

      Expected.Action_List := new Action_Node'
        (Symbol  => Token_Pkg.Terminal_ID'Last, -- ignored, since this is the last action
         Action  => new Parse_Action_Node'
           (Item => (Verb => Error),
            Next => null),
         Next  => null);

      Conflict := new Parse_Action_Node'
        (Item           =>
           (Verb        => Reduce,
            LHS         => Get_Production (6).LHS,
            Action      => null,
            Index       => 0,
            Token_Count => 0),
         Next           => null);

      Conflict := new Parse_Action_Node'
        (Item     =>
           (Verb  => Shift,
            State => 3),
         Next     => Conflict);

      Expected.Action_List := new Action_Node'
        (Symbol      => IDENTIFIER_ID,
         Action      => Conflict,
         Next        => Expected.Action_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => label_opt_ID,
         State  => 6,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => sequence_of_statements_ID,
         State  => 5,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => statement_ID,
         State  => 4,
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

      Parser := Generators.Generate
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
