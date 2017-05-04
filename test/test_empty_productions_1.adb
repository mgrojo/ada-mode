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

with AUnit.Checks;
with Ada.Text_IO;
with FastToken.Lexer;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.LR1_Generator;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Test_Empty_Productions_1 is

   --  A grammar with a null production in a list (from ../wisi/test/empty_production_1.wy)

   type Token_ID is
     (
      --  non-reporting
      Whitespace_ID,
      COMMENT_ID,

      --  terminals
      BEGIN_ID,
      IS_ID,
      SEMICOLON_ID,
      EOF_ID,

      --  non-terminals
      fasttoken_accept_ID,
      declarations_ID,
      declarative_part_ID,
      body_ID);

   package Token_Pkg is new FastToken.Token (Token_ID, BEGIN_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Token_Pkg.Nonterminal;
   package Production is new FastToken.Production (Token_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Token_Pkg);
   package Parser_Root is new FastToken.Parser (Token_Pkg, EOF_ID, fasttoken_accept_ID, Lexer_Root);
   First_State_Index : constant := 1;
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package LALR_Generator is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);
   package LR1_Generator is new LR.LR1_Generator (Production, LR1_Items, Generator_Utils);

   --  Allow infix operators for building productions
   use all type Production.Instance;
   use all type Production.List.Instance;
   use all type Production.Right_Hand_Side;
   use all type Token_Pkg.List.Instance;

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     fasttoken_accept_ID <= declarative_part_ID & EOF_ID + Self -- 1
     and
     declarative_part_ID <= +Self -- 2
     and
     declarative_part_ID <= declarations_ID + Self -- 3
     and
     declarations_ID <= body_ID + Self -- 4
     and
     declarations_ID <= declarations_ID & body_ID + Self -- 5
     and
     body_ID <= IS_ID & declarative_part_ID & BEGIN_ID & SEMICOLON_ID + Self -- 6
     ;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, BEGIN_ID, EOF_ID, Token_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, 1, LR, LR1_Items, Grammar);

   Has_Empty_Production : constant LR1_Items.Nonterminal_ID_Set :=
     LR1_Items.Has_Empty_Production (Grammar);

   First : constant LR1_Items.Derivation_Matrix := LR1_Items.First
     (Grammar, Has_Empty_Production, Trace => False);

   procedure Test_Closure
     (Prod     : in Integer;
      Symbol   : in Token_Pkg.Terminal_ID;
      Expected : in LR1_Items.Item_Set;
      Debug    : in Boolean)
   is
      use Ada.Text_IO;
      use LR1_Items;
      use FastToken_AUnit;

      Label : constant String := Integer'Image (Prod) & "." & Token_ID'Image (Symbol);

      Computed : constant LR1_Items.Item_Set := LR1_Items.Closure
        ((Set             => new LR1_Items.Item_Node'
            (LR1_Items.Item_Node_Of
               (FastToken_AUnit.Get_Production (Prod),
                LR.Unknown_State,
                +Symbol)),
          Goto_List       => null,
          State           => LR.Unknown_State,
          Next            => null),
         Has_Empty_Production, First, Grammar,
         Trace            => Debug);

   begin
      if Debug then
         Put_Line ("label:   " & Label);
         Put_Line ("expected: "); Put (Expected);
         Put_Line ("computed: "); Put (Computed);
      end if;

      Check (Label, Computed, Expected);

   end Test_Closure;

   procedure Test_Goto_Transitions
     (Label    : in String;
      Kernel   : in LR1_Items.Item_Set;
      Symbol   : in Token_ID;
      Expected : in LR1_Items.Item_Set;
      Debug    : in Boolean)
   is
      use Ada.Text_IO;
      use LR1_Items;
      use FastToken_AUnit;
      Computed : Item_Set := LALR_Generator.LALR_Goto_Transitions (Kernel, Symbol, First, Grammar, Debug);
   begin
      if Debug then
         Put_Line ("symbol:   " & Token_ID'Image (Symbol));
         Put ("expected: "); Put (Expected);
         Put ("LALR computed: "); Put (Computed);
      end if;

      Check (Label & ",LALR", Computed, Expected);

      Computed := LR1_Generator.LR1_Goto_Transitions (Kernel, Symbol, Has_Empty_Production, First, Grammar, Debug);
      if Debug then
         Put ("LR1 computed: "); Put (Computed);
      end if;

      Check (Label & ",LR1", Computed, Expected);

   end Test_Goto_Transitions;

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
        (Kernel.all, Has_Empty_Production, First, Grammar, Trace => False);
      Conflicts : Generator_Utils.Conflict_Lists.List;
      Table     : LR.Parse_Table (1 .. Kernels.Size);
   begin
      if Debug then
         LR1_Items.Put (Kernel.all);
      end if;

      Generator_Utils.Add_Actions
        (Closure, Table, Has_Empty_Production, Conflicts, Trace => Debug);

      LR1_Items.Free (Closure);

      Check (Label, Table (Kernel.State), Expected);
   end Test_Actions;

   ----------
   --  Test procedures

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use AUnit.Checks;
   begin
      Check ("1", First (declarations_ID)(IS_ID), True);
   end Test_First;

   procedure Test_Closure (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use FastToken_AUnit;
      use LR1_Items;

   begin
      --  Match each test to [A -> alpha Dot B Beta, a]
      --  All have Dot at beginning, so alpha = empty,
      --  'a' is only relevent when Beta is empty

      Test_Closure
        --  A = fasttoken_accept_ID, B = declarative_part_ID, Beta = EOF_ID, a = irrelevant
        (1, BEGIN_ID, Debug =>  Test.Debug,
         Expected =>
           +(Get_Item (1, 0, +BEGIN_ID) &
               Get_Item (2, 0, +EOF_ID) &
               Get_Item (3, 0, +EOF_ID) &
               Get_Item (4, 0, +(IS_ID, EOF_ID)) &
               Get_Item (5, 0, +(IS_ID, EOF_ID)) &
               Get_Item (6, 0, +(IS_ID, EOF_ID))));

      --  Closure on 1, 0 * gives the same result, with * in the first item lookahead.

      Test_Closure
        --  A = declarative_part_ID, B = empty, Beta = empty, a = BEGIN_ID
        (2, BEGIN_ID, Debug =>  Test.Debug,
         Expected => +(Get_Item (2, 0, +BEGIN_ID)));

      --  Closure on 2, 0 * gives the same result, with * in the lookahead.

      Test_Closure
        --  A = declarative_part_ID, B = declarations_ID, Beta = empty, a = BEGIN_ID
        --  Same as tail of 1, 0, BEGIN_ID, starting at prod 3.
        (3, BEGIN_ID, Debug =>  Test.Debug,
         Expected =>
           +(Get_Item (3, 0, +BEGIN_ID) &
               Get_Item (4, 0, +(BEGIN_ID, IS_ID)) &
               Get_Item (5, 0, +(BEGIN_ID, IS_ID)) &
               Get_Item (6, 0, +(BEGIN_ID, IS_ID))
               ));

      Test_Closure
        --  A = declarative_part_ID, B = declarations_ID, Beta = empty, a = IS_ID
        (3, IS_ID, Debug =>  Test.Debug,
         Expected =>
           +(Get_Item (3, 0, +IS_ID) &
               Get_Item (4, 0, +IS_ID) &
               Get_Item (5, 0, +IS_ID) &
               Get_Item (6, 0, +IS_ID)
               ));

      --  Closure on 3, 0 SEMICOLON_ID/EOF_ID gives the same result as 1, 0 BEGIN_ID, with the given lookahead.

      Test_Closure
        --  A = declarative_part_ID, B = body_ID, Beta = empty, a = BEGIN_ID
        (4, BEGIN_ID, Debug =>  Test.Debug,
         Expected =>
           +(Get_Item (4, 0, +BEGIN_ID) &
               Get_Item (6, 0, +BEGIN_ID)
               ));

      --  Closure on 4, 0 * gives the same result as 4, 0 BEGIN_ID, with the given lookahead.

      Test_Closure
        --  A = declarations_ID, B = declarations_ID, Beta = body_ID, a = irrelevant
        (5, IS_ID, Debug =>  Test.Debug,
         Expected =>
           +(Get_Item (5, 0, +IS_ID) &
               Get_Item (4, 0, +IS_ID) &
               Get_Item (6, 0, +IS_ID)
               ));

      Test_Closure
        --  A = body_ID, B = IS_ID, Beta = irrelevant, a = irrelevant
        (6, IS_ID, Debug =>  Test.Debug,
         Expected =>
           +(Get_Item (6, 0, +IS_ID)
               ));

   end Test_Closure;

   procedure Goto_Transitions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR1_Items;
      use FastToken_AUnit;

      --  kernel:
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 6, -- in grammar
         Dot  => 2,
         Next => null);

      Expected : Item_Set;

   begin
      if Test.Debug then
         Ada.Text_IO.Put ("kernel 1: "); Put (Kernel);
      end if;

      --  Expected goto_transitions on IS_ID (nested declaration):
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 6, -- in Grammar
         Dot  => 2,
         Next => null);

      Test_Goto_Transitions ("1", Kernel, IS_ID, Expected, Test.Debug);

      --  Expected goto_transitions on BEGIN_ID: none

      Expected :=
        (Set       => null,
         Goto_List => null,
         State     => LR.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("2", Kernel, BEGIN_ID, Expected, Test.Debug);

      --  Expected goto_transitions on SEMICOLON_ID: none

      Test_Goto_Transitions ("3", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_1;

   procedure Goto_Transitions_2 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR1_Items;
      use FastToken_AUnit;

      --  kernel:
      --  BODY_ID <= IS_ID DECLARATIVE_PART_ID ^ BEGIN_ID SEMICOLON_ID

      Kernel : constant Item_Set := Get_Item_Set
        (Prod => 6, -- in grammar
         Dot  => 3,
         Next => null);

      Expected : Item_Set;

   begin
      if Test.Debug then
         Ada.Text_IO.Put ("kernel 2: "); Put (Kernel);
      end if;

      --  Expected goto_transitions on BEGIN_ID:
      --  BODY_ID <= IS_ID DECLARATIVE_PART_ID BEGIN_ID ^ SEMICOLON_ID

      Expected := Get_Item_Set
        (Prod => 6, -- in Grammar
         Dot  => 4,
         Next => null);

      Test_Goto_Transitions ("1", Kernel, BEGIN_ID, Expected, Test.Debug);

      --  Expected goto_transitions on SEMICOLON_ID: none

      Expected :=
        (Set       => null,
         Goto_List => null,
         State     => LR.Unknown_State,
         Next      => null);

      Test_Goto_Transitions ("2", Kernel, SEMICOLON_ID, Expected, Test.Debug);

   end Goto_Transitions_2;

   procedure Goto_Set_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR1_Items;
      use FastToken_AUnit;

      Kernels  : constant Item_Set_List := LALR_Generator.LALR_Kernels
        (Grammar, First, Trace => False, First_State_Index => 1);
      Kernel   : constant Item_Set_Ptr  := Find (2, Kernels);
      Expected : Goto_Item_Ptr;

   begin
      --  Kernel 2:
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID

      --  Expected goto_list:
      --  BODY_ID => DECLARATIONS_ID <= BODY_ID ^ ; Set 5
      --  DECLARATIVE_PART_ID => BODY_ID <= IS_ID DECLARATIVE_PART_ID ^ BEGIN_ID SEMICOLON_ID ; Set 7
      --  DECLARATIONS_ID =>
      --    DECLARATIVE_PART_ID <= DECLARATIONS_ID ^
      --    DECLARATIONS_ID <= DECLARATIONS_ID ^ BODY_ID ; Set 3
      --  IS_ID => BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID ; Set 2

      --  Only Index is checked in Expected.Set
      Expected := new Goto_Item'
        (Symbol => IS_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 2, Next => null),
         Next   => Expected);

      Expected := new Goto_Item'
        (Symbol => declarations_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 3, Next => null),
         Next   => Expected);

      Expected := new Goto_Item'
        (Symbol => declarative_part_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 7, Next => null),
         Next   => Expected);

      Expected := new Goto_Item'
        (Symbol => body_ID,
         Set    => new Item_Set'(Set => null, Goto_List => null, State => 5, Next => null),
         Next   => Expected);

      if Test.Debug then
         Ada.Text_IO.Put_Line ("kernels:"); Put (Kernels);
         Ada.Text_IO.Put_Line ("Expected:"); Put (Expected);
      end if;

      Check ("", Kernel.Goto_List, Expected);

   end Goto_Set_1;

   procedure Actions_1 (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);
      use LR;
      use LR1_Items;
      use FastToken_AUnit;

      Kernels : constant Item_Set_List := LALR_Generator.LALR_Kernels
        (Grammar, First, Trace => False, First_State_Index => 1);

      Expected : Parse_State;
   begin
      --  Kernel 2:
      --
      --  BODY_ID <= IS_ID ^ DECLARATIVE_PART_ID BEGIN_ID SEMICOLON_ID
      --
      --  Expected actions:
      --  BEGIN_ID => reduce 0 tokens to declarative_part_id
      --  IS_ID    => shift and goto 2
      --  default  => ERROR

      --  Expected reduction gotos:
      --  DECLARATIONS_ID => Set 3
      --  DECLARATIVE_PART_ID => Set 7
      --  BODY_ID => Set 5
      Expected.Action_List := new Action_Node'
        (Symbol  => Token_Pkg.Terminal_ID'Last, -- ignored, since this is the last action
         Action  => new Parse_Action_Node'
           (Item => (Verb => Error),
            Next => null),
         Next  => null);

      Expected.Action_List := new Action_Node'
        (Symbol            => BEGIN_ID,
         Action            => new Parse_Action_Node'
           (Item           =>
              (Verb        => Reduce,
               LHS         => Get_Production (4).LHS,
               Action      => null,
               Index       => 0,
               Token_Count => 0),
            Next           => null),
         Next              => Expected.Action_List);

      Expected.Action_List := new Action_Node'
        (Symbol      => IS_ID,
         Action      => new Parse_Action_Node'
           (Item     =>
              (Verb  => Shift,
               State => 2),
            Next     => null),
         Next        => Expected.Action_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => body_ID,
         State  => 5,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => declarative_part_ID,
         State  => 7,
         Next   => Expected.Goto_List);

      Expected.Goto_List := new Goto_Node'
        (Symbol => declarations_ID,
         State  => 3,
         Next   => Expected.Goto_List);

      Test_Actions ("1", Kernels, 2, Expected, Test.Debug);

   end Actions_1;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_empty_productions_1.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Test_Closure'Access, "Debug");
      else
         Register_Routine (T, Test_First'Access, "Test_First");
         Register_Routine (T, Test_Closure'Access, "Test_Closure");
         Register_Routine (T, Goto_Transitions_1'Access, "Goto_Transitions_1");
         Register_Routine (T, Goto_Transitions_2'Access, "Goto_Transitions_2");
         Register_Routine (T, Goto_Set_1'Access, "Goto_Set_1");
         Register_Routine (T, Actions_1'Access, "Actions_1");
      end if;
   end Register_Tests;

end Test_Empty_Productions_1;
