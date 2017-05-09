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
with FastToken.Parser.LR.LALR_Generator;
with FastToken.Parser.LR.Parser;
with FastToken.Parser.LR.Parser_Lists;
with FastToken.Parser.LR1_Items;
with FastToken.Production;
with FastToken.Text_Feeder.String;
with FastToken.Token.Nonterminal;
with Gen_FastToken_AUnit;
package body Dragon_4_45_LALR_Test is

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
   package Parser_Root is new FastToken.Parser
     (Token_ID, Token_ID'First, EOF_ID, EOF_ID, Accept_ID, Token_ID'Image, Ada.Text_IO.Put, Tokens_Pkg, Lexer_Root);
   package LR is new Parser_Root.LR (First_State_Index, Token_ID'Width, Nonterminal, Nonterminal.Get);
   package LR1_Items is new Parser_Root.LR1_Items
     (LR.Unknown_State_Index, LR.Unknown_State, LR.Nonterminal_Pkg, Production);
   package Generator_Utils is new LR.Generator_Utils (Production, LR1_Items);
   package Generators is new LR.LALR_Generator (Production, LR1_Items, Generator_Utils);

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

   --  See comment in Test_LALR_Kernels about state numbering
   S0  : constant := 0;
   S1  : constant := 3;
   S2  : constant := 4;
   S36 : constant := 1;
   S47 : constant := 2;
   S5  : constant := 5;
   S89 : constant := 6;

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

   ----------
   --  Test procedures

   First : constant LR1_Items.Derivation_Matrix := LR1_Items.First
     (Grammar, Has_Empty_Production => (others => False), Trace => False);

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      --  FIRST defined in dragon pg 45

      Expected : constant LR1_Items.Derivation_Matrix :=
        (Accept_ID  => (Upper_S_ID | Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
         Upper_S_ID => (Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
         Upper_C_ID => (Lower_C_ID | Lower_D_ID => True, others => False));
   begin
      Check ("1", First, Expected);
   end Test_First;

   procedure Test_LALR_Kernels (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use LR1_Items;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Item_Set_List := Generators.LALR_Kernels
        (Grammar, First, Trace => Test.Debug, First_State_Index => First_State_Index);

      Expected : constant Item_Set_List :=
      --  [dragon] example 4.42 pg 233 shows the item sets.
      --  LALR_Kernels computes the combined kernels of these (see page
      --  240). The LALR states and gotos are shown in fig 4.41 page 239.
      --
      --  In addition, the example does a depth-first search for
      --  new sets; we do a breadth first search; so the numbering of
      --  states is different. In this test, we accomodate that by
      --  using symbolic names matching the example state labels, and
      --  adding kernels to the list in the order we compute them.
        (S0 + Get_Item (1, 1, Null_Lookaheads)) &
        (S36 + Get_Item (3, 2, Null_Lookaheads)) &
        (S47 + Get_Item (4, 2, Null_Lookaheads)) &
        (S1 + Get_Item (1, 2, Null_Lookaheads)) &
        (S2 + Get_Item (2, 2, Null_Lookaheads)) &
        (S5 + Get_Item (2, 3, Null_Lookaheads)) &
        (S89 + Get_Item (3, 3, Null_Lookaheads));

   begin
      Add_Gotos
        (Expected, S0,
         +(Lower_C_ID, Get_Set (S36, Expected)) &
           (Lower_D_ID, Get_Set (S47, Expected)) &
           (Upper_S_ID, Get_Set (S1, Expected)) &
           (Upper_C_ID, Get_Set (S2, Expected)));

      Add_Gotos
        (Expected, S2,
         +(Lower_C_ID, Get_Set (S36, Expected)) &
           (Lower_D_ID, Get_Set (S47, Expected)) &
           (Upper_C_ID, Get_Set (S5, Expected)));

      Add_Gotos
        (Expected, S36,
         +(Lower_C_ID, Get_Set (S36, Expected)) &
           (Lower_D_ID, Get_Set (S47, Expected)) &
           (Upper_C_ID, Get_Set (S89, Expected)));

      if Test.Debug then
         Ada.Text_IO.Put_Line ("computed:");
         Put (Computed);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Put (Expected);
      end if;
      Check ("", Computed, Expected);
   end Test_LALR_Kernels;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use LR;

      Test : Test_Case renames Test_Case (T);

      Computed : constant LR.Parse_Table_Ptr := Generators.Generate (Grammar, Put_Parse_Table => Test.Debug);
      Expected : LR.Parse_Table (0 .. 6);

   begin
      --  figure 4.41 pg 239

      Add_Action (Expected (S0), Lower_C_ID, S36);
      Add_Action (Expected (S0), Lower_D_ID, S47);
      Add_Error (Expected (S0));
      Add_Goto (Expected (S0), Upper_C_ID, S2);
      Add_Goto (Expected (S0), Upper_S_ID, S1);

      Add_Action (Expected (S1), EOF_ID, LR.Accept_It, Accept_ID, 1, Self);
      Add_Error (Expected (S1));

      Add_Action (Expected (S2), Lower_C_ID, S36);
      Add_Action (Expected (S2), Lower_D_ID, S47);
      Add_Error (Expected (S2));
      Add_Goto (Expected (S2), Upper_C_ID, S5);

      Add_Action (Expected (S36), Lower_C_ID, S36);
      Add_Action (Expected (S36), Lower_D_ID, S47);
      Add_Error (Expected (S36));
      Add_Goto (Expected (S36), Upper_C_ID, S89);

      Add_Action (Expected (S47), Lower_C_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S47), Lower_D_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Action (Expected (S47), EOF_ID, LR.Reduce, Upper_C_ID, 1, Self);
      Add_Error (Expected (S47));

      Add_Action (Expected (S5), EOF_ID, LR.Reduce, Upper_S_ID, 2, Self);
      Add_Error (Expected (S5));

      Add_Action (Expected (S89), Lower_C_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S89), Lower_D_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Action (Expected (S89), EOF_ID, LR.Reduce, Upper_C_ID, 2, Self);
      Add_Error (Expected (S89));

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
      return new String'("../../Test/dragon_4_45_lalr_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Test_Parse'Access, "debug");
      else
         Register_Routine (T, Test_First'Access, "Test_First");
         Register_Routine (T, Test_LALR_Kernels'Access, "Test_LALR_Kernels");
         Register_Routine (T, Parser_Table'Access, "Parser_Table");
         Register_Routine (T, Test_Parse'Access, "Test_Parse");
      end if;
   end Register_Tests;

end Dragon_4_45_LALR_Test;
