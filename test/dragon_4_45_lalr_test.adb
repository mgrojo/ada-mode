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
with WisiToken.Gen_Token_Enum;
with WisiToken.Lexer.Regexp;
with WisiToken.Parser.LR.AUnit;
with WisiToken.Parser.LR.LALR_Generator;
with WisiToken.Parser.LR.LR1_Items;
with WisiToken.Parser.LR.Parser;
with WisiToken.Production;
with WisiToken.Text_Feeder.String;
with WisiToken.Text_IO_Trace;
with WisiToken.AUnit;
with WisiToken_AUnit; use WisiToken_AUnit;
package body Dragon_4_45_LALR_Test is

   --  grammar in eqn (4.21) example 4.42 pg 231

   type Token_Enum_ID is
     (
      --  terminals
      Lower_C_ID,
      Lower_D_ID,
      EOF_ID,

      --  non-terminals
      Accept_ID,
      Upper_S_ID,
      Upper_C_ID);

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_Enum_ID,
      First_Terminal    => Lower_C_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Accept_ID,
      Last_Nonterminal  => Upper_C_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Accept_ID);
   use Token_Enum;

   First_State_Index  : constant := 0;
   First_Parser_Label : constant := 1;

   --  Allow infix operators for building productions
   use all type WisiToken.Production.Right_Hand_Side;
   use all type WisiToken.Production.List.Instance;

   Null_Action : WisiToken.Semantic_Action renames WisiToken.Null_Action;

   Grammar : constant WisiToken.Production.List.Instance :=
     Accept_ID <= Upper_S_ID & EOF_ID + Null_Action -- 1
     and
     Upper_S_ID <= Upper_C_ID & Upper_C_ID + Null_Action -- 2
     and
     Upper_C_ID <= Lower_C_ID & Upper_C_ID + Null_Action -- 3
     and
     Upper_C_ID <= Lower_D_ID + Null_Action -- 4
     ;

   --  See comment in Test_LALR_Kernels about state numbering
   S0  : constant := 0;
   S1  : constant := 3;
   S2  : constant := 4;
   S36 : constant := 1;
   S47 : constant := 2;
   S5  : constant := 5;
   S89 : constant := 6;

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Lower_C_ID => Lexer.Get ("c"),
       Lower_D_ID => Lexer.Get ("d"),
       EOF_ID     => Lexer.Get ("" & WisiToken.EOF_Character)
     ));

   String_Feeder : aliased WisiToken.Text_Feeder.String.Instance;

   Has_Empty_Production : constant WisiToken.Token_ID_Set :=
     WisiToken.Parser.LR.LR1_Items.Has_Empty_Production (Grammar, LALR_Descriptor);

   First : constant WisiToken.Token_Array_Token_Set := WisiToken.Parser.LR.LR1_Items.First
     (Grammar, LALR_Descriptor, Has_Empty_Production, Trace => False);

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LALR_Descriptor'Access);
   State : State_Type (Trace'Access, LR1_Descriptor.First_Terminal, LR1_Descriptor.Last_Terminal);

   ----------
   --  Test procedures

   procedure Test_First (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      --  FIRST defined in dragon pg 45

      Expected : constant WisiToken.Token_Array_Token_Set := To_Nonterminal_Array_Token_Set
        ((Accept_ID  => (Upper_S_ID | Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_S_ID => (Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID => True, others => False)));
   begin
      Check ("1", First, Expected);
   end Test_First;

   procedure Test_LALR_Kernels (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use WisiToken.Parser.LR.LR1_Items;
      use all type WisiToken.Token_ID;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Item_Set_List := WisiToken.Parser.LR.LALR_Generator.LALR_Kernels
        (Grammar, First, First_State_Index, LALR_Descriptor, Trace => Test.Debug);

      Null_Lookaheads : constant WisiToken.Token_ID_Set :=
        --  + 1 for propagate
        (LALR_Descriptor.First_Terminal .. LALR_Descriptor.Last_Terminal + 1 => False);

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
        (S0 + Get_Item (Grammar, 1, 1, Null_Lookaheads)) &
        (S36 + Get_Item (Grammar, 3, 2, Null_Lookaheads)) &
        (S47 + Get_Item (Grammar, 4, 2, Null_Lookaheads)) &
        (S1 + Get_Item (Grammar, 1, 2, Null_Lookaheads)) &
        (S2 + Get_Item (Grammar, 2, 2, Null_Lookaheads)) &
        (S5 + Get_Item (Grammar, 2, 3, Null_Lookaheads)) &
        (S89 + Get_Item (Grammar, 3, 3, Null_Lookaheads));

   begin
      Add_Gotos
        (Expected, S0,
         +(+Lower_C_ID, Get_Set (S36, Expected)) &
           (+Lower_D_ID, Get_Set (S47, Expected)) &
           (+Upper_S_ID, Get_Set (S1, Expected)) &
           (+Upper_C_ID, Get_Set (S2, Expected)));

      Add_Gotos
        (Expected, S2,
         +(+Lower_C_ID, Get_Set (S36, Expected)) &
           (+Lower_D_ID, Get_Set (S47, Expected)) &
           (+Upper_C_ID, Get_Set (S5, Expected)));

      Add_Gotos
        (Expected, S36,
         +(+Lower_C_ID, Get_Set (S36, Expected)) &
           (+Lower_D_ID, Get_Set (S47, Expected)) &
           (+Upper_C_ID, Get_Set (S89, Expected)));

      if Test.Debug then
         Ada.Text_IO.Put_Line ("computed:");
         Put (LALR_Descriptor, Computed);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Put (LALR_Descriptor, Expected);
      end if;
      Check ("", Computed, Expected);
   end Test_LALR_Kernels;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use WisiToken.Parser.LR;
      use WisiToken.Parser.LR.AUnit;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Parse_Table_Ptr := LALR_Generator.Generate
        (Grammar, LALR_Descriptor, First_State_Index, Put_Parse_Table => Test.Debug);

      Expected : Parse_Table
        (State_First       => 0,
         State_Last        => 6,
         First_Terminal    => +Lower_C_ID,
         Last_Terminal     => +EOF_ID,
         First_Nonterminal => +Accept_ID,
         Last_Nonterminal  => +Upper_C_ID);

   begin
      Expected.Panic_Recover := (others => False);
      Expected.Follow := To_Nonterminal_Array_Terminal_Set
        ((Accept_ID  => (others => False),
          Upper_S_ID => (EOF_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID | EOF_ID => True, others => False)));

      --  figure 4.41 pg 239

      Add_Action (Expected.States (S0), +Lower_C_ID, S36);
      Add_Action (Expected.States (S0), +Lower_D_ID, S47);
      Add_Error (Expected.States (S0));
      Add_Goto (Expected.States (S0), +Upper_C_ID, S2);
      Add_Goto (Expected.States (S0), +Upper_S_ID, S1);

      Add_Action (Expected.States (S1), +EOF_ID, Accept_It, +Accept_ID, 0, 1, Null_Action);
      Add_Error (Expected.States (S1));

      Add_Action (Expected.States (S2), +Lower_C_ID, S36);
      Add_Action (Expected.States (S2), +Lower_D_ID, S47);
      Add_Error (Expected.States (S2));
      Add_Goto (Expected.States (S2), +Upper_C_ID, S5);

      Add_Action (Expected.States (S36), +Lower_C_ID, S36);
      Add_Action (Expected.States (S36), +Lower_D_ID, S47);
      Add_Error (Expected.States (S36));
      Add_Goto (Expected.States (S36), +Upper_C_ID, S89);

      Add_Action (Expected.States (S47), +Lower_C_ID, Reduce, +Upper_C_ID, 0, 1, Null_Action);
      Add_Action (Expected.States (S47), +Lower_D_ID, Reduce, +Upper_C_ID, 0, 1, Null_Action);
      Add_Action (Expected.States (S47), +EOF_ID, Reduce, +Upper_C_ID, 0, 1, Null_Action);
      Add_Error (Expected.States (S47));

      Add_Action (Expected.States (S5), +EOF_ID, Reduce, +Upper_S_ID, 0, 2, Null_Action);
      Add_Error (Expected.States (S5));

      Add_Action (Expected.States (S89), +Lower_C_ID, Reduce, +Upper_C_ID, 0, 2, Null_Action);
      Add_Action (Expected.States (S89), +Lower_D_ID, Reduce, +Upper_C_ID, 0, 2, Null_Action);
      Add_Action (Expected.States (S89), +EOF_ID, Reduce, +Upper_C_ID, 0, 2, Null_Action);
      Add_Error (Expected.States (S89));

      if Test.Debug then
         --  computed output above
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line ("expected:");
         Put (LALR_Descriptor, Expected);
      end if;

      Check ("", Computed.all, Expected);
   end Parser_Table;

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Parser : WisiToken.Parser.LR.Parser.Instance := WisiToken.Parser.LR.Parser.New_Parser
        (Lexer.New_Lexer (Trace'Access, Syntax, String_Feeder'Access),
         WisiToken.Parser.LR.LALR_Generator.Generate (Grammar, LALR_Descriptor, First_State_Index, Trace => Test.Debug),
         State,
         First_Parser_Label);

      procedure Execute_Command (Command : in String)
      is
         use Ada.Exceptions;
      begin
         String_Feeder.Set (Command);

         Parser.Lexer.Reset (Buffer_Size => Command'Length + 1); -- +1 for EOF

         Parser.Parse;
      exception
      when E : others =>
         AUnit.Assertions.Assert (False, "'" & Command & "': " & Exception_Name (E) & ": " & Exception_Message (E));
      end Execute_Command;

   begin
      WisiToken.Trace_Parse := (if Test.Debug then 2 else 0);

      Execute_Command ("cdcd");
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("../Test/dragon_4_45_lalr_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Test_LALR_Kernels'Access, "debug");
      else
         Register_Routine (T, Test_First'Access, "Test_First");
         Register_Routine (T, Test_LALR_Kernels'Access, "Test_LALR_Kernels");
         Register_Routine (T, Parser_Table'Access, "Parser_Table");
         Register_Routine (T, Test_Parse'Access, "Test_Parse");
      end if;
   end Register_Tests;

end Dragon_4_45_LALR_Test;
