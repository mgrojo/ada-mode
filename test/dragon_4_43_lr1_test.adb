--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
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

with AUnit.Assertions;
with Ada.Exceptions;
with Ada.Text_IO;
with FastToken.Lexer.Regexp;
with FastToken.Parser.LR.Generator_Utils;
with FastToken.Parser.LR.LR1_Generator;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Text_Feeder.String;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Dragon_4_43_LR1_Test is

   --  grammar in eqn (4.21) example 4.42 pg 231

   type Token_ID is
     (
      --  terminals
      Lower_C_ID,
      Lower_D_ID,
      EOF_ID,

      --  non-terminals
      Accept_ID,
      Upper_S_ID,
      Upper_C_ID);

   First_State_Index : constant := 0;
   package Tokens_Pkg is new FastToken.Token (Token_ID, Lower_C_ID, EOF_ID, Token_ID'Image);
   package Nonterminal is new Tokens_Pkg.Nonterminal;
   package Production is new FastToken.Production (Tokens_Pkg, Nonterminal);
   package Lexer_Root is new FastToken.Lexer (Tokens_Pkg);
   package Parser_Root is new FastToken.Parser (Tokens_Pkg, EOF_ID, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package Generators is new LR.LR1_Generator (Production, LR1_Items, Generator_Utils);

   --  Allow infix operators for building productions
   use type Tokens_Pkg.List.Instance;
   use type Production.Right_Hand_Side;
   use type Production.Instance;
   use type Production.List.Instance;

   function "+" (Item : in Token_ID) return Tokens_Pkg.Instance'Class renames Tokens_Pkg."+";

   Self : Nonterminal.Synthesize renames Nonterminal.Synthesize_Self;

   Grammar : constant Production.List.Instance :=
     Nonterminal.Get (Accept_ID) <= (+Upper_S_ID) & (+EOF_ID) + Self -- 1
     and
     Nonterminal.Get (Upper_S_ID) <= (+Upper_C_ID) & (+Upper_C_ID) + Self -- 2
     and
     Nonterminal.Get (Upper_C_ID) <= (+Lower_C_ID) & (+Upper_C_ID) + Self -- 3
     and
     Nonterminal.Get (Upper_C_ID) <= (+Lower_D_ID) + Self -- 4
     ;

   package Lexer is new Lexer_Root.Regexp;
   First_Parser_Label : constant := 1;
   package Parser_Lists is new LR.Parser_Lists (First_Parser_Label);
   package LR_Parser is new LR.Parser (First_Parser_Label, Parser_Lists => Parser_Lists);

   Syntax : constant Lexer.Syntax :=
     (
      Lower_C_ID => Lexer.Get ("c", +Lower_C_ID),
      Lower_D_ID => Lexer.Get ("d", +Lower_D_ID),
      EOF_ID     => Lexer.Get ("" & FastToken.EOF_Character, +EOF_ID)
     );

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;

   package FastToken_AUnit is new Gen_FastToken_AUnit
     (Token_ID, Lower_C_ID, EOF_ID, Tokens_Pkg, Nonterminal, Production,
      Lexer_Root, Parser_Root, First_State_Index, LR, LR1_Items, Grammar);
   use FastToken_AUnit;

   Has_Empty_Production : constant LR1_Items.Nonterminal_ID_Set :=
     LR1_Items.Has_Empty_Production (Grammar);

   First : constant LR1_Items.Derivation_Matrix := LR1_Items.First
     (Grammar, Has_Empty_Production, Trace => False);

   function Close
     (Item  : in LR1_Items.Item_Ptr;
      State : in LR.State_Index)
     return LR1_Items.Item_Set_Ptr
   is
      use LR1_Items;
   begin
      Item.State := State;
      return new Item_Set'
        (Closure
           ((Item, null, State, null), Has_Empty_Production, First, Grammar,
            Match_Lookaheads => False,
            Trace => False));
   end Close;

   ----------
   --  Test procedures

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      --  FIRST defined in dragon pg 45

      Expected : constant LR1_Items.Derivation_Matrix :=
        (Accept_ID  => (Upper_S_ID | Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
         Upper_S_ID => (Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
         Upper_C_ID => (Lower_C_ID | Lower_D_ID => True, others => False));
   begin
      Check ("0", Has_Empty_Production, LR1_Items.Nonterminal_ID_Set'(others => False));
      Check ("1", First, Expected);
   end Test_First;

   procedure Test_Closure (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use LR1_Items;

      Test : Test_Case renames Test_Case (T);

      procedure One
        (Label    : in String;
         Item     : in Item_Ptr;
         Expected : in Item_Set)
      is
         Computed : constant Item_Set := Closure
           (Item_Set'
              (Set       => Item,
               Goto_List => null,
               State     => LR.Unknown_State,
               Next      => null),
            Has_Empty_Production, First, Grammar, Match_Lookaheads => False, Trace => Test.Debug);

      begin
         if Test.Debug then
            Ada.Text_IO.Put_Line (Label & ".computed:");
            Put (Computed, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line (Label & ".expected:");
            Put (Expected, Show_Lookaheads => True);
         end if;
         Check (Label, Computed, Expected);
      end One;

      Expected : Item_Set;

   begin
      Expected.State := LR.Unknown_State;

      --  [dragon] pg 233 - 234 gives the closures as part of the discussion

      --  close [S' -> . S, $]
      --  Add in Computed order.
      Add (Get_Item_Node (1, 1, +EOF_ID).all, Expected);
      Add (Get_Item_Node (2, 1, +EOF_ID).all, Expected);
      Add (Get_Item_Node (3, 1, +(Lower_D_ID, Lower_C_ID)).all, Expected);
      Add (Get_Item_Node (4, 1, +(Lower_D_ID, Lower_C_ID)).all, Expected);

      One ("1", Get_Item_Node (1, 1, +EOF_ID), Expected);

      --  close [C -> c . C, c/d]
      Expected := (null, null, LR.Unknown_State, null);
      Add (Get_Item_Node (3, 2, +(Lower_D_ID, Lower_C_ID)).all, Expected);
      Add (Get_Item_Node (3, 1, +(Lower_C_ID, Lower_D_ID)).all, Expected);
      Add (Get_Item_Node (4, 1, +(Lower_C_ID, Lower_D_ID)).all, Expected);

      One ("2", Get_Item_Node (3, 2, +(Lower_C_ID, Lower_D_ID)), Expected);
   end Test_Closure;

   procedure Test_Goto (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use LR1_Items;

      Test : Test_Case renames Test_Case (T);

      procedure One
        (Label    : in String;
         Set      : in Item_Set;
         Symbol   : in Token_ID;
         Expected : in Item_Set)
      is
         Computed : constant Item_Set := Generators.LR1_Goto_Transitions
           (Set, Symbol, Has_Empty_Production, First, Grammar, Test.Debug);
      begin
         if Test.Debug then
            Ada.Text_IO.Put_Line (Label & ".computed:");
            Put (Computed, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line (Label & ".expected:");
            Put (Expected, Show_Lookaheads => True);
         end if;
         Check (Label, Computed, Expected);
      end One;

      I0 : constant Item_Set := Closure
        (Get_Item_Set (1, 1, +EOF_ID),
         Has_Empty_Production, First, Grammar, Match_Lookaheads => False, Trace => False);

      Expected : Item_Set;

   begin
      Expected.State := LR.Unknown_State;

      if Test.Debug then
         Ada.Text_IO.Put_Line ("I0:");
         Put (I0, Show_Lookaheads => True);
      end if;

      --  [dragon] pg 233 - 234 gives the gotos as part of the discussion

      --  goto I0, S
      Add (Get_Item_Node (1, 2, +EOF_ID).all, Expected);
      One ("1", I0, Upper_S_ID, Expected);

      --  goto I0, C
      Expected := (null, null, LR.Unknown_State, null);
      Add (Get_Item_Node (2, 2, +EOF_ID).all, Expected);
      Add (Get_Item_Node (3, 1, +EOF_ID).all, Expected);
      Add (Get_Item_Node (4, 1, +EOF_ID).all, Expected);
      One ("2", I0, Upper_C_ID, Expected);

      --  goto I0, $ = null
      Expected := (null, null, LR.Unknown_State, null);
      One ("3", I0, EOF_ID, Expected);

      --  goto I0, c
      Expected := (null, null, LR.Unknown_State, null);
      Add (Get_Item_Node (3, 2, +(Lower_C_ID, Lower_D_ID)).all, Expected);
      Add (Get_Item_Node (3, 1, +(Lower_D_ID, Lower_C_ID)).all, Expected);
      Add (Get_Item_Node (4, 1, +(Lower_D_ID, Lower_C_ID)).all, Expected);

      One ("4", I0, Lower_C_ID, Expected);

      --  goto I0, d
      Expected := (null, null, LR.Unknown_State, null);
      Add (Get_Item_Node (4, 2, +(Lower_C_ID, Lower_D_ID)).all, Expected);
      One ("5", I0, Lower_D_ID, Expected);
   end Test_Goto;

   procedure Test_LR1_Items (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use LR1_Items;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Item_Set_List := Generators.LR1_Item_Sets
        (Has_Empty_Production, First, Grammar, First_State_Index, Trace => Test.Debug);

      Expected : Item_Set_Ptr;

      --  [dragon] pg 232, 233 shows the item sets in the discussion.
      --  The states and gotos are shown in fig 4.40 page 236.

      --  In addition, the example iterates over tokens in a different
      --  order, so the numbering of states is different. In this
      --  test, we accomodate that by using symbolic names matching
      --  the example state labels, and pushing kernels on the list in
      --  the order they appear in the example.

      I0 : constant Item_Set_Ptr := Close (Get_Item_Node (1, 1, +EOF_ID), 0);
      I1 : constant Item_Set_Ptr := Close (Get_Item_Node (1, 2, +EOF_ID), 3);
      I2 : constant Item_Set_Ptr := Close (Get_Item_Node (2, 2, +EOF_ID), 4);
      I3 : constant Item_Set_Ptr := Close (Get_Item_Node (3, 2, +(Lower_D_ID, Lower_C_ID)), 1);
      I4 : constant Item_Set_Ptr := Close (Get_Item_Node (4, 2, +(Lower_D_ID, Lower_C_ID)), 2);
      I5 : constant Item_Set_Ptr := Close (Get_Item_Node (2, 3, +EOF_ID), 7);
      I6 : constant Item_Set_Ptr := Close (Get_Item_Node (3, 2, +EOF_ID), 5);
      I7 : constant Item_Set_Ptr := Close (Get_Item_Node (4, 2, +EOF_ID), 6);
      I8 : constant Item_Set_Ptr := Close (Get_Item_Node (3, 3, +(Lower_C_ID, Lower_D_ID)), 8);
      I9 : constant Item_Set_Ptr := Close (Get_Item_Node (3, 3, +EOF_ID), 9);

      Goto_0 : constant Goto_Item_Ptr :=
        (Upper_C_ID, I2, null) &
        (Upper_S_ID, I1, null) &
        (Lower_D_ID, I4, null) &
        (Lower_C_ID, I3, null);

      Goto_2 : constant Goto_Item_Ptr :=
        (Upper_C_ID, I5, null) &
        (Lower_D_ID, I7, null) &
        (Lower_C_ID, I6, null);

      Goto_3 : constant Goto_Item_Ptr :=
        (Upper_C_ID, I8, null) &
        (Lower_D_ID, I4, null) &
        (Lower_C_ID, I3, null);

      Goto_6 : constant Goto_Item_Ptr :=
        (Upper_C_ID, I9, null) &
        (Lower_D_ID, I7, null) &
        (Lower_C_ID, I6, null);
   begin
      --  Computed has I0 last in list; push in Computed (= State) order.
      Expected := new Item_Set'(I0.Set, Goto_0, 0, Expected);
      Expected := new Item_Set'(I3.Set, Goto_3, 1, Expected);
      Expected := new Item_Set'(I4.Set, null,   2, Expected);
      Expected := new Item_Set'(I1.Set, null,   3, Expected);
      Expected := new Item_Set'(I2.Set, Goto_2, 4, Expected);
      Expected := new Item_Set'(I6.Set, Goto_6, 5, Expected);
      Expected := new Item_Set'(I7.Set, null,   6, Expected);
      Expected := new Item_Set'(I5.Set, null,   7, Expected);
      Expected := new Item_Set'(I8.Set, null,   8, Expected);
      Expected := new Item_Set'(I9.Set, null,   9, Expected);

      if Test.Debug then
         Ada.Text_IO.Put_Line ("computed:");
         Put (Computed, Show_Lookaheads => True);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Put (Item_Set_List'(Expected, 10), Show_Lookaheads => True);
      end if;
      Check ("", Computed.Head, Expected);
   end Test_LR1_Items;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use LR;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Parse_Table_Ptr := Generators.Generate (Grammar, Put_Parse_Table => Test.Debug);
      Expected : Parse_Table (0 .. 9);

      --  See comment in Test_LR1_Items about state numbering

      S0 : constant := 0;
      S1 : constant := 3;
      S2 : constant := 4;
      S3 : constant := 1;
      S4 : constant := 2;
      S5 : constant := 7;
      S6 : constant := 5;
      S7 : constant := 6;
      S8 : constant := 8;
      S9 : constant := 9;

   begin
      --  figure 4.41 pg 239
      --  'r1' means reduce by production 1, 0 indexed; our production 2
      --  'acc' = reduce by our production 1

      Add_Action (Expected (S0), Lower_D_ID, S4);
      Add_Action (Expected (S0), Lower_C_ID, S3);
      Add_Action (Expected (S0), Tokens_Pkg.Terminal_ID'Last); -- default = error
      Add_Goto (Expected (S0), Upper_C_ID, S2);
      Add_Goto (Expected (S0), Upper_S_ID, S1);

      Add_Action (Expected (S1), EOF_ID, LR.Accept_It, Accept_ID, 1, Self);
      Add_Action (Expected (S1), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S2), Lower_D_ID, S7);
      Add_Action (Expected (S2), Lower_C_ID, S6);
      Add_Action (Expected (S2), Tokens_Pkg.Terminal_ID'Last); -- default = error
      Add_Goto (Expected (S2), Upper_C_ID, S5);

      Add_Action (Expected (S3), Lower_D_ID, S4);
      Add_Action (Expected (S3), Lower_C_ID, S3);
      Add_Action (Expected (S3), Tokens_Pkg.Terminal_ID'Last); -- default = error
      Add_Goto (Expected (S3), Upper_C_ID, S8);

      Add_Action (Expected (S4), Lower_C_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S4), Lower_D_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S4), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S5), EOF_ID, LR.Reduce, Upper_S_ID, 2, Self);
      Add_Action (Expected (S5), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S6), Lower_D_ID, S7);
      Add_Action (Expected (S6), Lower_C_ID, S6);
      Add_Action (Expected (S6), Tokens_Pkg.Terminal_ID'Last); -- default = error
      Add_Goto (Expected (S6), Upper_C_ID, S9);

      Add_Action (Expected (S7), EOF_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S7), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S8), Lower_C_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S8), Lower_D_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S8), Tokens_Pkg.Terminal_ID'Last); -- default = error

      Add_Action (Expected (S9), EOF_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S9), Tokens_Pkg.Terminal_ID'Last); -- default = error

      if Test.Debug then
         --  computed output above
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line ("expected:");
         LR.Put (Expected);
      end if;

      FastToken_AUnit.Check ("", Computed.all, Expected);
   end Parser_Table;

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Parser : LR_Parser.Instance := LR_Parser.Initialize
        (Lexer.Initialize (Syntax, String_Feeder'Access),
         Generators.Generate (Grammar, Trace => Test.Debug));

      procedure Execute_Command (Command : in String)
      is begin
         String_Feeder.Set (Command);

         Parser.Reset (Buffer_Size => Command'Length + 1); -- +1 for EOF

         Parser.Parse;
      exception
      when E : others =>
         AUnit.Assertions.Assert (False, "'" & Command & "': " & Ada.Exceptions.Exception_Message (E));
      end Execute_Command;

   begin
      FastToken.Trace_Parse := (if Test.Debug then 2 else 0);

      Execute_Command ("cdcd");
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../../Test/dragon_4_43_lr1_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Test_Parse'Access, "debug");
      else
         Register_Routine (T, Test_First'Access, "Test_First");
         Register_Routine (T, Test_Closure'Access, "Test_Closure");
         Register_Routine (T, Test_Goto'Access, "Test_Goto");
         Register_Routine (T, Test_LR1_Items'Access, "Test_LR1_Items");
         Register_Routine (T, Parser_Table'Access, "Parser_Table");
         Register_Routine (T, Test_Parse'Access, "Test_Parse");
      end if;
   end Register_Tests;

end Dragon_4_43_LR1_Test;
