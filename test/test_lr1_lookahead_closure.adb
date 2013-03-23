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

with AUnit.Check;
with OpenToken.Production.List;
with OpenToken.Production.Parser.LRk_Item;
with OpenToken.Production.Parser;
with OpenToken.Token.Enumerated.Analyzer;
with OpenToken.Token.Enumerated.List;
with OpenToken.Token.Enumerated.Nonterminal;
package body Test_LR1_Lookahead_Closure is

   --  A grammar for a greatly simplified form of the Ada 2012 case
   --  expression; this exposed a bug in Lookahead_Closure. Captured
   --  from one version of ../wisi/test/case_expression.wy.

   type Token_IDs is
     (
      --  non-reporting
      Whitespace_ID,
      COMMENT_ID,

      --  terminals
      RANGE_ID,
      WHEN_ID,
      DOT_DOT_ID,
      EQUAL_GREATER_ID,
      TICK_ID,
      IDENTIFIER_ID,
      EOF_ID,

      --  non-terminals
      opentoken_accept_ID,
      case_expression_ID,
      choice_expression_ID,
      choice_relation_ID,
      discrete_choice_ID,
      factor_ID,
      factor_list_ID,
      range_nt_ID);
   package Tokens_Pkg is new OpenToken.Token.Enumerated (Token_IDs, Token_IDs'Image, Token_IDs'Width);
   package Analyzers is new Tokens_Pkg.Analyzer
     (First_Terminal => RANGE_ID,
      Last_Terminal  => EOF_ID);
   package Token_Lists is new Tokens_Pkg.List;
   package Nonterminals is new Tokens_Pkg.Nonterminal (Token_Lists);
   package Productions is new OpenToken.Production (Tokens_Pkg, Token_Lists, Nonterminals);
   package Production_Lists is new Productions.List;
   package Parsers is new Productions.Parser (Production_Lists, Analyzers);

   function "+"
     (Item : in Token_IDs)
     return Tokens_Pkg.Instance'Class
   renames Tokens_Pkg."+";

   --  Allow infix operators for building productions
   use type Token_Lists.Instance;
   use type Productions.Right_Hand_Side;
   use type Productions.Instance;
   use type Production_Lists.Instance;

   Self : Nonterminals.Synthesize renames Nonterminals.Synthesize_Self;

   Grammar : constant Production_Lists.Instance :=
     --  1
     Nonterminals.Get (opentoken_accept_ID) <= Nonterminals.Get (case_expression_ID) & (+EOF_ID)
     and -- 2
     Nonterminals.Get (case_expression_ID) <= (+WHEN_ID) & (+discrete_choice_ID) & (+EQUAL_GREATER_ID) + Self
     and -- 3
     Nonterminals.Get (choice_expression_ID) <= (+choice_relation_ID) + Self
     and -- 4
     Nonterminals.Get (choice_relation_ID) <= (+factor_list_ID) + Self
     and -- 5
     Nonterminals.Get (discrete_choice_ID) <= (+choice_expression_ID) + Self
     and -- 6
     Nonterminals.Get (discrete_choice_ID) <= (+range_nt_ID) + Self
     and -- 7
     Nonterminals.Get (factor_ID) <= (+IDENTIFIER_ID) + Self
     and -- 8
     Nonterminals.Get (factor_list_ID) <= (+factor_ID) + Self
     and -- 9
     Nonterminals.Get (range_nt_ID) <= (+IDENTIFIER_ID) & (+TICK_ID) & (+RANGE_ID) + Self
     and -- 10
     Nonterminals.Get (range_nt_ID) <= (+factor_list_ID) & (+DOT_DOT_ID) & (+factor_list_ID) + Self
     ;

   package LR1 is new Parsers.LRk_Item (1);

   procedure Check is new AUnit.Check.Gen_Check_Discrete (Token_IDs);

   procedure Check
     (Label    : in String;
      Computed : in Token_Lists.List_Iterator;
      Expected : in Token_Lists.List_Iterator)
   is
      use AUnit.Check;
      use Tokens_Pkg;
      use Productions;
      use Token_Lists;
      Computed_I : List_Iterator := Computed;
      Expected_I : List_Iterator := Expected;
      Index      : Integer       := 1;
   begin
      loop
         if Computed_I = Null_Iterator or Expected_I = Null_Iterator then
            Check (Label & " = null", Computed_I = Null_Iterator and Expected_I = Null_Iterator, True);
            exit;
         end if;
         Check (Label & Integer'Image (Index), ID (Computed_I), ID (Expected_I));
         Next_Token (Computed_I);
         Next_Token (Expected_I);
         Index := Index + 1;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Productions.Instance;
      Expected : in Productions.Instance)
   is
      use AUnit.Check;
      use Productions;
   begin
      Check (Label & ".Index", Index (Computed), Index (Expected));
      Check (Label & ".LHS", LHS_ID (Computed), LHS_ID (Expected));
      Check (Label & ".RHS", First_Token (Computed), First_Token (Expected));
   end Check;

   procedure Check (Label : in String; Computed : in LR1.Item_Lookahead_Ptr; Expected : in LR1.Item_Lookahead_Ptr)
   is
      use AUnit.Check;
      use LR1;
      Computed_I : Item_Lookahead_Ptr := Computed;
      Expected_I : Item_Lookahead_Ptr := Expected;
      Index : Integer := 1;
   begin
      loop
         if Computed_I = null then
            Check (Label & " = null", Expected_I = null, True);
            exit;
         end if;
         Check (Label & Integer'Image (Index), Computed_I.Lookaheads (1), Expected_I.Lookaheads (1));
         Check
           (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
      end loop;
   end Check;

   procedure Check (Label : in String; Computed : in LR1.Item_Ptr; Expected : in LR1.Item_Ptr)
   is
      use AUnit.Check;
      use LR1;
      Computed_I : Item_Ptr := Computed;
      Expected_I : Item_Ptr := Expected;
      Index      : Integer  := 1;
   begin
      loop
         Check (Label & Integer'Image (Index) & ".Index", Computed_I.Index, Expected_I.Index);
         Check (Label & Integer'Image (Index) & ".Prod", Computed_I.Prod, Expected_I.Prod);
         Check (Label & Integer'Image (Index) & ".Dot", Computed_I.Dot, Expected_I.Dot);
         Check (Label & Integer'Image (Index) & ".Lookaheads", Computed_I.Lookaheads, Expected_I.Lookaheads);
         Check (Label & Integer'Image (Index) & ".Next = null", Computed_I.Next = null, Expected_I.Next = null);
         Computed_I := Computed_I.Next;
         Expected_I := Expected_I.Next;
         Index      := Index + 1;
         exit when Computed_I = null;
      end loop;
   end Check;

   procedure Check (Label : in String; Computed : in LR1.Item_Set; Expected : in LR1.Item_Set)
   is
      use AUnit.Check;
      use LR1;
   begin
      Check (Label & ".Index", Computed.Index, Expected.Index);
      Check (Label & ".Set", Computed.Set, Expected.Set);
      Check (Label & ".Goto = null", Computed.Goto_List = null, True);
      Check (Label & ".Next = null", Computed.Next = null, Expected.Next = null);
   end Check;

   function Get_Item_Node
     (Prod       : in Integer;
      Lookaheads : in LR1.Item_Lookahead_Ptr;
      Next       : in LR1.Item_Ptr;
      Dot        : in Integer := 1)
     return LR1.Item_Ptr
   is
      Grammar_I : Production_Lists.List_Iterator := Grammar.Initial_Iterator;

      Dot_I : Token_Lists.List_Iterator;
   begin
      for I in 2 .. Prod loop
         Production_Lists.Next_Production (Grammar_I);
      end loop;

      Dot_I := Productions.First_Token (Production_Lists.Get_Production (Grammar_I));
      for I in 2 .. Dot loop
         Token_Lists.Next_Token (Dot_I);
      end loop;

      return new LR1.Item_Node'
        (Prod       => Production_Lists.Get_Production (Grammar_I),
         Dot        => Dot_I,
         Index      => -1,
         Lookaheads => Lookaheads,
         Next       => Next);
   end Get_Item_Node;

   type Token_Array is array (Positive range <>) of Token_IDs;

   function "+" (Item : in Token_Array) return LR1.Item_Lookahead_Ptr
   is
      use LR1;
      Result : Item_Lookahead_Ptr;
   begin
      for I in reverse Item'Range loop
         Result := new Item_Lookahead'
           (Last       => 1,
            Lookaheads => (1 => Item (I)),
            Next       => Result);
      end loop;
      return Result;
   end "+";

   ----------
   --  Test procedures

   procedure Nominal (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use LR1;

      First : constant Derivation_Matrix := First_Derivations (Grammar, Trace => Test.Debug);

      Kernels : Item_Set_List := LR0_Kernels (Grammar, First, Test.Debug, First_Index => 1);

      procedure Test_One (Label : in String; Input : in Item_Set; Expected : in Item_Set)
      is
         Closure : Item_Set_Ptr := new Item_Set'(Lookahead_Closure (Input, First, Grammar));
      begin
         if Test.Debug then
            LR1.Put (Closure.all);
         end if;
         Check (Label, Closure.all, Expected);
         Free (Closure.all);
         Free (Closure);
      end Test_One;

      Null_Item_Set : constant Item_Set :=
        (Set           => null,
         Goto_List     => null,
         Index         => -1,
         Next          => null);

      Expected : Item_Set;
   begin
      --  Set 1: OPENTOKEN_ACCEPT_ID <= ^ CASE_EXPRESSION_ID EOF_ID
      --  closure:
      --  CASE_EXPRESSION_ID <= ^ WHEN_ID DISCRETE_CHOICE_ID EQUAL_GREATER_ID
      --  OPENTOKEN_ACCEPT_ID <= ^ CASE_EXPRESSION_ID EOF_ID
      Expected := Null_Item_Set;
      Expected.Set := Get_Item_Node
        (Prod       => 1,
         Lookaheads => null,
         Next       => Expected.Set);

      Expected.Set := Get_Item_Node (2, +((1 => EOF_ID)), Expected.Set);

      Test_One ("1", LR1.Find (1, Kernels).all, Expected);

      --  Set 2: CASE_EXPRESSION_ID <= WHEN_ID ^ DISCRETE_CHOICE_ID EQUAL_GREATER_ID
      Expected := Null_Item_Set;
      Expected.Set := Get_Item_Node (2, null, Expected.Set, Dot => 2);
      Expected.Set := Get_Item_Node (5, +((1 => EQUAL_GREATER_ID)), Expected.Set);
      Expected.Set := Get_Item_Node (6, +((1 => EQUAL_GREATER_ID)), Expected.Set);
      Expected.Set := Get_Item_Node (9, +((1 => EQUAL_GREATER_ID)), Expected.Set);
      Expected.Set := Get_Item_Node (10, +((1 => EQUAL_GREATER_ID)), Expected.Set);
      Expected.Set := Get_Item_Node (3, +((1 => EQUAL_GREATER_ID)), Expected.Set);
      Expected.Set := Get_Item_Node (4, +((1 => EQUAL_GREATER_ID)), Expected.Set);
      Expected.Set := Get_Item_Node (8, +((EQUAL_GREATER_ID, DOT_DOT_ID)), Expected.Set);
      Expected.Set := Get_Item_Node (7, +((EQUAL_GREATER_ID, DOT_DOT_ID)), Expected.Set);

      Test_One ("2", LR1.Find (2, Kernels).all, Expected);

      Free (Kernels);
   end Nominal;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/test_lr1_lookahead_closure.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

end Test_LR1_Lookahead_Closure;
