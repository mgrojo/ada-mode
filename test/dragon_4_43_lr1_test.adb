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
with FastToken.Gen_Token_Enum;
with FastToken.Lexer.Regexp;
with FastToken.Parser.LR.LR1_Generator;
with FastToken.Parser.LR.LR1_Items;
with FastToken.Parser.LR.Parser;
with FastToken.Production;
with FastToken.Text_Feeder.String;
with FastToken.Text_IO_Trace;
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

   package Token_Enum is new FastToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_ID,
      First_Terminal    => Lower_C_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Accept_ID,
      Last_Nonterminal  => Upper_C_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Accept_ID);
   use Token_Enum;

   First_State_Index  : constant := 0;
   First_Parser_Label : constant := 1;

   use all type FastToken.Production.Right_Hand_Side;
   use all type FastToken.Production.List.Instance;

   Null_Action : FastToken.Semantic_Action renames FastToken.Null_Action;

   Grammar : constant FastToken.Production.List.Instance :=
     --  [dragon] (2.21) pg 231
     Accept_ID <= Upper_S_ID & EOF_ID + Null_Action -- 1
     and
     Upper_S_ID <= Upper_C_ID & Upper_C_ID + Null_Action -- 2
     and
     Upper_C_ID <= Lower_C_ID & Upper_C_ID + Null_Action -- 3
     and
     Upper_C_ID <= Lower_D_ID + Null_Action -- 4
     ;

   Map : constant array (FastToken.Parser.LR.State_Index range 0 .. 9) of FastToken.Parser.LR.Unknown_State_Index :=
     --  Map (dragon index) = our index; see comment in Test_LR1_Items
     (0 => 0,
      1 => 3,
      2 => 4,
      3 => 1,
      4 => 2,
      5 => 7,
      6 => 5,
      7 => 6,
      8 => 8,
      9 => 9);

   package Lexer renames FastToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Lower_C_ID => Lexer.Get ("c"),
       Lower_D_ID => Lexer.Get ("d"),
       EOF_ID     => Lexer.Get ("" & FastToken.EOF_Character)
     ));

   String_Feeder : aliased FastToken.Text_Feeder.String.Instance;

   package FastToken_AUnit is new Gen_FastToken_AUnit (Grammar);
   use FastToken_AUnit;

   Has_Empty_Production : constant FastToken.Token_ID_Set :=
     FastToken.Parser.LR.LR1_Items.Has_Empty_Production (Grammar, LR1_Descriptor);

   First : constant FastToken.Token_Array_Token_Set := FastToken.Parser.LR.LR1_Items.First
     (Grammar, LR1_Descriptor, Has_Empty_Production, Trace => False);

   Trace : aliased FastToken.Text_IO_Trace.Trace (LALR_Descriptor'Access);
   State : State_Type (Trace'Access);

   ----------
   --  Test procedures

   procedure Test_First_Follow (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      --  FIRST defined in [dragon] pg 189; we add nonterminals

      Expected_First : constant FastToken.Token_Array_Token_Set := To_Nonterminal_Array_Token_Set
        ((Accept_ID  => (Upper_S_ID | Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_S_ID => (Upper_C_ID | Lower_C_ID | Lower_D_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID => True, others => False)));

      --  FOLLOW defined in [dragon] pg 189
      Expected_Follow : constant FastToken.Token_Array_Token_Set := To_Nonterminal_Array_Terminal_Set
        ((Accept_ID  => (others => False),
          Upper_S_ID => (EOF_ID => True, others => False),
          Upper_C_ID => (Lower_C_ID | Lower_D_ID | EOF_ID => True, others => False)));

      Computed_Follow : constant FastToken.Token_Array_Token_Set := FastToken.Parser.LR.LR1_Items.Follow
        (Grammar, LR1_Descriptor, First, Has_Empty_Production);
   begin
      Check ("0", Has_Empty_Production, FastToken.Token_ID_Set'(+Accept_ID .. +Upper_C_ID => False));
      Check ("1", First, Expected_First);

      if Test.Debug then
         Ada.Text_IO.Put_Line ("Follow:");
         FastToken.Put (LR1_Descriptor, Computed_Follow);
      end if;

      Check ("2", Computed_Follow, Expected_Follow);
   end Test_First_Follow;

   procedure Test_LR1_Items (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use FastToken.Parser.LR.LR1_Items;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Item_Set_List := FastToken.Parser.LR.LR1_Generator.LR1_Item_Sets
        (Has_Empty_Production, First, Grammar, First_State_Index, LR1_Descriptor, Trace => Test.Debug);

      Expected : constant Item_Set_List :=
        --  [dragon] fig 4.39 pg 235 shows the item sets and gotos. We
        --  search in a different order, which causes state numbers to
        --  not match, so we use Map.
        (Map (0) +
           (Get_Item (1, 1, +EOF_ID) &
              Get_Item (2, 1, +EOF_ID) &
              Get_Item (3, 1, +(Lower_D_ID, Lower_C_ID)) &
              Get_Item (4, 1, +(Lower_D_ID, Lower_C_ID)))) &
        (Map (3) +
           (Get_Item (3, 2, +(Lower_C_ID, Lower_D_ID)) &
              Get_Item (3, 1, +(Lower_C_ID, Lower_D_ID)) &
              Get_Item (4, 1, +(Lower_C_ID, Lower_D_ID)))) &
        (Map (4) +
           Get_Item (4, 2, +(Lower_C_ID, Lower_D_ID))) &
        (Map (1) +
           Get_Item (1, 2, +EOF_ID)) &
        (Map (2) +
           (Get_Item (2, 2, +EOF_ID) &
              Get_Item (3, 1, +EOF_ID) &
              Get_Item (4, 1, +EOF_ID))) &
        (Map (6) +
           (Get_Item (3, 2, +EOF_ID) &
              Get_Item (3, 1, +EOF_ID) &
              Get_Item (4, 1, +EOF_ID))) &
        (Map (7) +
           Get_Item (4, 2, +EOF_ID)) &
        (Map (5) +
           Get_Item (2, 3, +EOF_ID)) &
        (Map (8) +
           Get_Item (3, 3, +(Lower_C_ID, Lower_D_ID))) &
        (Map (9) +
           Get_Item (3, 3, +EOF_ID))
      ;

   begin
      Add_Gotos
        (Expected, Map (0),
         +(+Lower_C_ID, Get_Set (Map (3), Expected)) &
           (+Lower_D_ID, Get_Set (Map (4), Expected)) &
           (+Upper_S_ID, Get_Set (Map (1), Expected)) &
           (+Upper_C_ID, Get_Set (Map (2), Expected)));

      Add_Gotos
        (Expected, Map (2),
         +(+Lower_C_ID, Get_Set (Map (6), Expected)) &
           (+Lower_D_ID, Get_Set (Map (7), Expected)) &
           (+Upper_C_ID, Get_Set (Map (5), Expected)));

      Add_Gotos
        (Expected, Map (3),
         +(+Lower_C_ID, Get_Set (Map (3), Expected)) &
           (+Lower_D_ID, Get_Set (Map (4), Expected)) &
           (+Upper_C_ID, Get_Set (Map (8), Expected)));

      Add_Gotos
        (Expected, Map (6),
         +(+Lower_C_ID, Get_Set (Map (6), Expected)) &
           (+Lower_D_ID, Get_Set (Map (7), Expected)) &
           (+Upper_C_ID, Get_Set (Map (9), Expected)));

      if Test.Debug then
         Ada.Text_IO.Put_Line ("computed:");
         Put (LR1_Descriptor, Computed);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Put (LR1_Descriptor, Expected);
      end if;
      Check ("", Computed, Expected);
   end Test_LR1_Items;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use FastToken.Parser.LR;

      Test : Test_Case renames Test_Case (T);

      Computed : constant Parse_Table_Ptr := FastToken.Parser.LR.LR1_Generator.Generate
        (Grammar, LR1_Descriptor, First_State_Index, Put_Parse_Table => Test.Debug);

      Expected : Parse_Table
        (State_First       => 0,
         State_Last        => 9,
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
      --  'r1' means reduce by production 1, 0 indexed; our production 2
      --  'acc' = reduce by our production 1

      Add_Action (Expected.States (Map (0)), +Lower_C_ID, Map (3));
      Add_Action (Expected.States (Map (0)), +Lower_D_ID, Map (4));
      Add_Error (Expected.States (Map (0)));
      Add_Goto (Expected.States (Map (0)), +Upper_C_ID, Map (2));
      Add_Goto (Expected.States (Map (0)), +Upper_S_ID, Map (1));

      Add_Action (Expected.States (Map (1)), +EOF_ID, Accept_It, +Accept_ID, 0, 1, Null_Action);
      Add_Error (Expected.States (Map (1)));

      Add_Action (Expected.States (Map (2)), +Lower_C_ID, Map (6));
      Add_Action (Expected.States (Map (2)), +Lower_D_ID, Map (7));
      Add_Error (Expected.States (Map (2)));
      Add_Goto (Expected.States (Map (2)), +Upper_C_ID, Map (5));

      Add_Action (Expected.States (Map (3)), +Lower_C_ID, Map (3));
      Add_Action (Expected.States (Map (3)), +Lower_D_ID, Map (4));
      Add_Error (Expected.States (Map (3)));
      Add_Goto (Expected.States (Map (3)), +Upper_C_ID, Map (8));

      Add_Action (Expected.States (Map (4)), +Lower_C_ID, Reduce, +Upper_C_ID, 0, 1, Null_Action);
      Add_Action (Expected.States (Map (4)), +Lower_D_ID, Reduce, +Upper_C_ID, 0, 1, Null_Action);
      Add_Error (Expected.States (Map (4))); -- default = error

      Add_Action (Expected.States (Map (5)), +EOF_ID, Reduce, +Upper_S_ID, 0, 2, Null_Action);
      Add_Error (Expected.States (Map (5)));

      Add_Action (Expected.States (Map (6)), +Lower_C_ID, Map (6));
      Add_Action (Expected.States (Map (6)), +Lower_D_ID, Map (7));
      Add_Error (Expected.States (Map (6)));
      Add_Goto (Expected.States (Map (6)), +Upper_C_ID, Map (9));

      Add_Action (Expected.States (Map (7)), +EOF_ID, Reduce, +Upper_C_ID, 0, 1, Null_Action);
      Add_Error (Expected.States (Map (7)));

      Add_Action (Expected.States (Map (8)), +Lower_C_ID, Reduce, +Upper_C_ID, 0, 2, Null_Action);
      Add_Action (Expected.States (Map (8)), +Lower_D_ID, Reduce, +Upper_C_ID, 0, 2, Null_Action);
      Add_Error (Expected.States (Map (8)));

      Add_Action (Expected.States (Map (9)), +EOF_ID, Reduce, +Upper_C_ID, 0, 2, Null_Action);
      Add_Error (Expected.States (Map (9)));

      if Test.Debug then
         --  computed output above during generate
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line ("expected:");
         Put (LR1_Descriptor, Expected);
      end if;

      Check ("", Computed.all, Expected);
   end Parser_Table;

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      Parser : FastToken.Parser.LR.Parser.Instance := FastToken.Parser.LR.Parser.New_Parser
        (Lexer.New_Lexer (Syntax, String_Feeder'Access),
         FastToken.Parser.LR.LR1_Generator.Generate (Grammar, LR1_Descriptor, First_State_Index, Trace => Test.Debug),
         State,
         First_Parser_Label);

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
      return new String'("../Test/dragon_4_43_lr1_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      if T.Debug then
         Register_Routine (T, Parser_Table'Access, "debug");
      else
         Register_Routine (T, Test_First_Follow'Access, "Test_First_Follow");
         Register_Routine (T, Test_LR1_Items'Access, "Test_LR1_Items");
         Register_Routine (T, Parser_Table'Access, "Parser_Table");
         Register_Routine (T, Test_Parse'Access, "Test_Parse");
      end if;
   end Register_Tests;

end Dragon_4_43_LR1_Test;
