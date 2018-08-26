--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with Ada.Characters.Latin_1;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Generate;
with WisiToken.LR.AUnit;
with WisiToken.LR.LR1_Generate;
with WisiToken.LR.LR1_Items.AUnit; use WisiToken.LR.LR1_Items.AUnit;
with WisiToken.LR.LR1_Items;
with WisiToken.LR.Parser;
with WisiToken.Lexer.Regexp;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada; use WisiToken.Wisi_Ada;
package body Dragon_4_43_LR1_Test is

   --  [dragon] example 4.43 pg 235

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
      Accept_ID         => Accept_ID,
      Case_Insensitive  => False);
   use Token_Enum;

   Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

   Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
     Accept_ID <= Upper_S_ID & EOF_ID + Null_Action -- 1
     and
     Upper_S_ID <= Upper_C_ID & Upper_C_ID + Null_Action -- 2
     and
     (Upper_C_ID <= Lower_C_ID & Upper_C_ID + Null_Action -- 3
      or
        Lower_D_ID + Null_Action) -- 4
     ;

   Map : constant array (WisiToken.State_Index range 0 .. 9) of WisiToken.Unknown_State_Index :=
     --  Map (dragon index) = our index; see comment in Test_LR1_Items
     (0 => 0,
      1 => 3,
      2 => 4,
      3 => 1,
      4 => 2,
      5 => 8,
      6 => 6,
      7 => 7,
      8 => 5,
      9 => 9);

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Lower_C_ID => Lexer.Get ("c"),
       Lower_D_ID => Lexer.Get ("d"),
       EOF_ID     => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
     ));

   Has_Empty_Production    : constant WisiToken.Token_ID_Set                 :=
     WisiToken.Generate.Has_Empty_Production (Grammar);
   First_Nonterm_Set       : constant WisiToken.Token_Array_Token_Set        := WisiToken.Generate.First
     (Grammar, Has_Empty_Production, Token_Enum.LALR_Descriptor.First_Terminal);
   First_Terminal_Sequence : constant WisiToken.Token_Sequence_Arrays.Vector :=
     WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Token_Enum.LALR_Descriptor);

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LR1_Descriptor'Access);

   ----------
   --  Test procedures

   procedure Test_First_Follow (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use WisiToken.AUnit;

      --  FIRST defined in [dragon] pg 189; we add nonterminals

      Expected_First_Nonterm_Set : constant WisiToken.Token_Array_Token_Set := To_Nonterminal_Array_Token_Set
        ((Accept_ID  => (Upper_S_ID | Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_S_ID => (Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID => True, others => False)));

      --  FOLLOW defined in [dragon] pg 189
      Expected_Follow : constant WisiToken.Token_Array_Token_Set := To_Nonterminal_Array_Terminal_Set
        ((Accept_ID  => (others => False),
          Upper_S_ID => (EOF_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID | EOF_ID => True)));

      Computed_Follow : constant WisiToken.Token_Array_Token_Set := WisiToken.Generate.Follow
        (Grammar, LR1_Descriptor, First_Nonterm_Set, Has_Empty_Production);
   begin
      Check ("0", Has_Empty_Production, WisiToken.Token_ID_Set'(+Accept_ID .. +Upper_C_ID => False));
      Check ("1", First_Nonterm_Set, Expected_First_Nonterm_Set);

      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line ("Follow:");
         WisiToken.Put (LR1_Descriptor, Computed_Follow);
      end if;

      Check ("2", Computed_Follow, Expected_Follow);
   end Test_First_Follow;

   procedure Test_LR1_Items (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.LR.LR1_Items;

      Computed : constant Item_Set_List := WisiToken.LR.LR1_Generate.LR1_Item_Sets
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, LR1_Descriptor);

      Expected : Item_Set_List :=
        --  [dragon] fig 4.39 pg 235 shows the item sets and gotos. We
        --  search in a different order, which causes state numbers to
        --  not match, so we use Map, and list them in our search order.
        (Map (0) +
           (Get_Item (Grammar, (+Accept_ID, 0), 1, +EOF_ID) &
              Get_Item (Grammar, (+Upper_S_ID, 0), 1, +EOF_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +(Lower_D_ID, Lower_C_ID)) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +(Lower_D_ID, Lower_C_ID)))) &
        (Map (3) +
           (Get_Item (Grammar, (+Upper_C_ID, 0), 2, +(Lower_C_ID, Lower_D_ID)) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +(Lower_C_ID, Lower_D_ID)) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +(Lower_C_ID, Lower_D_ID)))) &
        (Map (4) +
           Get_Item (Grammar, (+Upper_C_ID, 1), 2, +(Lower_C_ID, Lower_D_ID))) &
        (Map (1) +
           Get_Item (Grammar, (+Accept_ID, 0), 2, +EOF_ID)) &
        (Map (2) +
           (Get_Item (Grammar, (+Upper_S_ID, 0), 2, +EOF_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +EOF_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +EOF_ID))) &
        (Map (8) +
           Get_Item (Grammar, (+Upper_C_ID, 0), 3, +(Lower_C_ID, Lower_D_ID))) &
        (Map (6) +
           (Get_Item (Grammar, (+Upper_C_ID, 0), 2, +EOF_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 0), 1, +EOF_ID) &
              Get_Item (Grammar, (+Upper_C_ID, 1), 1, +EOF_ID))) &
        (Map (7) +
           Get_Item (Grammar, (+Upper_C_ID, 1), 2, +EOF_ID)) &
        (Map (5) +
           Get_Item (Grammar, (+Upper_S_ID, 0), 3, +EOF_ID)) &
        (Map (9) +
           Get_Item (Grammar, (+Upper_C_ID, 0), 3, +EOF_ID))
      ;

   begin
      Add_Gotos
        (Expected, Map (0),
         +(+Lower_C_ID, Map (3)) &
           (+Lower_D_ID, Map (4)) &
           (+Upper_S_ID, Map (1)) &
           (+Upper_C_ID, Map (2)));

      Add_Gotos
        (Expected, Map (2),
         +(+Lower_C_ID, Map (6)) &
           (+Lower_D_ID, Map (7)) &
           (+Upper_C_ID, Map (5)));

      Add_Gotos
        (Expected, Map (3),
         +(+Lower_C_ID, Map (3)) &
           (+Lower_D_ID, Map (4)) &
           (+Upper_C_ID, Map (8)));

      Add_Gotos
        (Expected, Map (6),
         +(+Lower_C_ID, Map (6)) &
           (+Lower_D_ID, Map (7)) &
           (+Upper_C_ID, Map (9)));

      if WisiToken.Trace_Action > WisiToken.Outline then
         Ada.Text_IO.Put_Line ("computed:");
         Put (Grammar, LR1_Descriptor, Computed);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Put (Grammar, LR1_Descriptor, Expected);
      end if;
      Check ("", Computed, Expected);
   end Test_LR1_Items;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      use WisiToken.LR;
      use WisiToken.LR.AUnit;

      Computed : constant Parse_Table_Ptr := WisiToken.LR.LR1_Generate.Generate (Grammar, LR1_Descriptor);

      Expected : Parse_Table
        (State_First       => 0,
         State_Last        => 9,
         First_Terminal    => +Lower_C_ID,
         Last_Terminal     => +EOF_ID,
         First_Nonterminal => +Accept_ID,
         Last_Nonterminal  => +Upper_C_ID);

      procedure Add_Action
        (State       : in out Parse_State;
         Symbol      : in     WisiToken.Token_ID;
         State_Index : in     WisiToken.State_Index)
      is begin
         Add_Action (State, Symbol, State_Index);
      end Add_Action;

      procedure Add_Action
        (State           : in out Parse_State;
         Symbol          : in     WisiToken.Token_ID;
         Verb            : in     Parse_Action_Verbs;
         LHS_ID          : in     WisiToken.Token_ID;
         RHS_Index       : in     Natural;
         RHS_Token_Count : in     Ada.Containers.Count_Type)
      is begin
         Add_Action (State, Symbol, Verb, (LHS_ID, RHS_Index), RHS_Token_Count, null, null);
      end Add_Action;

   begin
      --  figure 4.41 pg 239
      --  'r1' means reduce by production 1, 0 indexed; our production 2
      --  'acc' = reduce by our production 1

      Add_Action (Expected.States (Map (0)), +Lower_C_ID, Map (3));
      Add_Action (Expected.States (Map (0)), +Lower_D_ID, Map (4));
      Add_Error (Expected.States (Map (0)));
      Add_Goto (Expected.States (Map (0)), +Upper_C_ID, Map (2));
      Add_Goto (Expected.States (Map (0)), +Upper_S_ID, Map (1));

      Add_Action (Expected.States (Map (1)), +EOF_ID, Accept_It, +Accept_ID, 0, 1);
      Add_Error (Expected.States (Map (1)));

      Add_Action (Expected.States (Map (2)), +Lower_C_ID, Map (6));
      Add_Action (Expected.States (Map (2)), +Lower_D_ID, Map (7));
      Add_Error (Expected.States (Map (2)));
      Add_Goto (Expected.States (Map (2)), +Upper_C_ID, Map (5));

      Add_Action (Expected.States (Map (3)), +Lower_C_ID, Map (3));
      Add_Action (Expected.States (Map (3)), +Lower_D_ID, Map (4));
      Add_Error (Expected.States (Map (3)));
      Add_Goto (Expected.States (Map (3)), +Upper_C_ID, Map (8));

      Add_Action (Expected.States (Map (4)), +Lower_C_ID, Reduce, (+Upper_C_ID, 1), 1, Null_Action, null);
      Add_Action (Expected.States (Map (4)), +Lower_D_ID, Reduce, (+Upper_C_ID, 1), 1, Null_Action, null);
      Add_Error (Expected.States (Map (4))); -- default = error

      Add_Action (Expected.States (Map (5)), +EOF_ID, Reduce, (+Upper_S_ID, 0), 2, Null_Action, null);
      Add_Error (Expected.States (Map (5)));

      Add_Action (Expected.States (Map (6)), +Lower_C_ID, Map (6));
      Add_Action (Expected.States (Map (6)), +Lower_D_ID, Map (7));
      Add_Error (Expected.States (Map (6)));
      Add_Goto (Expected.States (Map (6)), +Upper_C_ID, Map (9));

      Add_Action (Expected.States (Map (7)), +EOF_ID, Reduce, (+Upper_C_ID, 1), 1, Null_Action, null);
      Add_Error (Expected.States (Map (7)));

      Add_Action (Expected.States (Map (8)), +Lower_C_ID, Reduce, (+Upper_C_ID, 0), 2, Null_Action, null);
      Add_Action (Expected.States (Map (8)), +Lower_D_ID, Reduce, (+Upper_C_ID, 0), 2, Null_Action, null);
      Add_Error (Expected.States (Map (8)));

      Add_Action (Expected.States (Map (9)), +EOF_ID, Reduce, (+Upper_C_ID, 0), 2, Null_Action, null);
      Add_Error (Expected.States (Map (9)));

      Check ("", Computed.all, Expected);
   end Parser_Table;

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Parser : WisiToken.LR.Parser.Parser;

      procedure Execute_Command (Command : in String)
      is begin
         Parser.Lexer.Reset_With_String (Command);
         Parser.Parse;
      exception
      when E : others =>
         AUnit.Assertions.Assert (False, "'" & Command & "': " & Ada.Exceptions.Exception_Message (E));
      end Execute_Command;

   begin
      WisiToken.LR.Parser.New_Parser
        (Parser,
         Trace'Access,
         Lexer.New_Lexer (Trace'Access, Syntax),
         WisiToken.LR.LR1_Generate.Generate (Grammar, LR1_Descriptor),
         User_Data                             => null,
         Language_Fixes                        => null,
         Language_Use_Minimal_Complete_Actions => null,
         Language_String_ID_Set                => null);

      Execute_Command ("cdcd");
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("dragon_4_43_lr1_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_First_Follow'Access, "Test_First_Follow");
      Register_Routine (T, Test_LR1_Items'Access, "Test_LR1_Items");
      Register_Routine (T, Parser_Table'Access, "Parser_Table");
      Register_Routine (T, Test_Parse'Access, "Test_Parse");
   end Register_Tests;

end Dragon_4_43_LR1_Test;
