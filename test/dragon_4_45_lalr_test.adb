--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Stephen Leake.  All Rights Reserved.
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
with Ada.Exceptions;
with Ada.Text_IO;
with WisiToken.AUnit;
with WisiToken.Gen_Token_Enum;
with WisiToken.Generate.LR.LALR_Generate;
with WisiToken.Generate.LR1_Items.AUnit; use WisiToken.Generate.LR1_Items.AUnit;
with WisiToken.Generate.LR1_Items;
with WisiToken.Lexer.Regexp;
with WisiToken.Parse.LR.AUnit;
with WisiToken.Parse.LR.Parser;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada; use WisiToken.Wisi_Ada;
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
      Accept_ID         => Accept_ID,
      Case_Insensitive  => False);
   use Token_Enum;

   Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

   Grammar : WisiToken.Productions.Prod_Arrays.Vector :=
     Accept_ID <= Upper_S_ID & EOF_ID + Null_Action -- 1
     and
     Upper_S_ID <= Upper_C_ID & Upper_C_ID + Null_Action -- 2
     and
     (Upper_C_ID <= Lower_C_ID & Upper_C_ID + Null_Action -- 3.0
      or
                    Lower_D_ID + Null_Action)             -- 3.1
     ;

   --  See comment in Test_LALR_Kernels about state numbering
   S0  : constant := 0;
   S1  : constant := 3;
   S2  : constant := 4;
   S36 : constant := 1;
   S47 : constant := 2;
   S5  : constant := 6;
   S89 : constant := 5;

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Lower_C_ID => Lexer.Get ("c"),
       Lower_D_ID => Lexer.Get ("d"),
       EOF_ID     => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
     ));


   Has_Empty_Production : constant WisiToken.Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Grammar);

   First : constant WisiToken.Token_Array_Token_Set := WisiToken.Generate.First
     (Grammar, Has_Empty_Production, LALR_Descriptor.First_Terminal);

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LR1_Descriptor'Access);

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
      use WisiToken.Generate.LR1_Items;
      use all type WisiToken.Token_ID;

      pragma Unreferenced (T);

      Computed : constant Item_Set_List := WisiToken.Generate.LR.LALR_Generate.LALR_Kernels
        (Grammar, First, LALR_Descriptor);

      Expected : Item_Set_List :=
      --  [dragon] example 4.42 pg 233 shows the item sets.
      --  LALR_Kernels computes the combined kernels of these (see page
      --  240). The LALR states and gotos are shown in fig 4.41 page 239.
      --
      --  In addition, the example does a depth-first search for
      --  new sets; we do a breadth first search; so the numbering of
      --  states is different. In this test, we accomodate that by
      --  using symbolic names matching the example state labels, and
      --  adding kernels to the list in the order we compute them.
        (S0 + Get_Item (Grammar, (+Accept_ID, 0), 1, Null_Lookahead)) &
        (S36 + Get_Item (Grammar, (+Upper_C_ID, 0), 2, Null_Lookahead)) &
        (S47 + Get_Item (Grammar, (+Upper_C_ID, 1), 2, Null_Lookahead)) &
        (S1 + Get_Item (Grammar, (+Accept_ID, 0), 2, Null_Lookahead)) &
        (S2 + Get_Item (Grammar, (+Upper_S_ID, 0), 2, Null_Lookahead)) &
        (S89 + Get_Item (Grammar, (+Upper_C_ID, 0), 3, Null_Lookahead)) &
        (S5 + Get_Item (Grammar, (+Upper_S_ID, 0), 3, Null_Lookahead));

   begin
      Add_Gotos
        (Expected, S0,
         +(+Lower_C_ID, S36) &
           (+Lower_D_ID, S47) &
           (+Upper_S_ID, S1) &
           (+Upper_C_ID, S2));

      Add_Gotos
        (Expected, S2,
         +(+Lower_C_ID, S36) &
           (+Lower_D_ID, S47) &
           (+Upper_C_ID, S5));

      Add_Gotos
        (Expected, S36,
         +(+Lower_C_ID, S36) &
           (+Lower_D_ID, S47) &
           (+Upper_C_ID, S89));

      if WisiToken.Trace_Generate_Table > WisiToken.Detail then
         Ada.Text_IO.Put_Line ("computed:");
         Put (Grammar, LALR_Descriptor, Computed);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("expected:");
         Put (Grammar, LALR_Descriptor, Expected);
      end if;
      Check ("", Computed, Expected);
   end Test_LALR_Kernels;

   procedure Parser_Table (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use WisiToken;
      use WisiToken.Parse.LR;
      use WisiToken.Parse.LR.AUnit;

      pragma Unreferenced (T);

      Computed : constant Parse_Table_Ptr := WisiToken.Generate.LR.LALR_Generate.Generate
        (Grammar, LALR_Descriptor, Grammar_File_Name => "");

      Expected : Parse_Table
        (State_First       => 0,
         State_Last        => 6,
         First_Terminal    => +Lower_C_ID,
         Last_Terminal     => +EOF_ID,
         First_Nonterminal => +Accept_ID,
         Last_Nonterminal  => +Upper_C_ID);

   begin
      --  figure 4.41 pg 239
      WisiToken.Parse.LR.AUnit.Strict := False;

      Add_Action (Expected.States (S0), +Lower_C_ID, (3, 0), S36);
      Add_Action (Expected.States (S0), +Lower_D_ID, (3, 1), S47);
      Add_Goto (Expected.States (S0), +Upper_C_ID, S2);
      Add_Goto (Expected.States (S0), +Upper_S_ID, S1);

      Add_Action (Expected.States (S1), +EOF_ID, Accept_It, (+Accept_ID, 0), 1, Null_Action, null);

      Add_Action (Expected.States (S2), +Lower_C_ID, (3, 0), S36);
      Add_Action (Expected.States (S2), +Lower_D_ID, (3, 1), S47);
      Add_Goto (Expected.States (S2), +Upper_C_ID, S5);

      Add_Action (Expected.States (S36), +Lower_C_ID, (3, 0), S36);
      Add_Action (Expected.States (S36), +Lower_D_ID, (3, 1), S47);
      Add_Goto (Expected.States (S36), +Upper_C_ID, S89);

      Add_Action (Expected.States (S47), +Lower_C_ID, Reduce, (+Upper_C_ID, 1), 1, Null_Action, null);
      Add_Action (Expected.States (S47), +Lower_D_ID, Reduce, (+Upper_C_ID, 1), 1, Null_Action, null);
      Add_Action (Expected.States (S47), +EOF_ID, Reduce, (+Upper_C_ID, 1), 1, Null_Action, null);

      Add_Action (Expected.States (S5), +EOF_ID, Reduce, (+Upper_S_ID, 0), 2, Null_Action, null);

      Add_Action (Expected.States (S89), +Lower_C_ID, Reduce, (+Upper_C_ID, 0), 2, Null_Action, null);
      Add_Action (Expected.States (S89), +Lower_D_ID, Reduce, (+Upper_C_ID, 0), 2, Null_Action, null);
      Add_Action (Expected.States (S89), +EOF_ID, Reduce, (+Upper_C_ID, 0), 2, Null_Action, null);

      Check ("", Computed.all, Expected);
   end Parser_Table;

   procedure Test_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);

      Parser : WisiToken.Parse.LR.Parser.Parser;

      procedure Execute_Command (Command : in String)
      is
         use Ada.Exceptions;
      begin
         Parser.Lexer.Reset_With_String (Command);
         Parser.Parse;
      exception
      when E : others =>
         AUnit.Assertions.Assert (False, "'" & Command & "': " & Exception_Name (E) & ": " & Exception_Message (E));
      end Execute_Command;

   begin
      WisiToken.Parse.LR.Parser.New_Parser
        (Parser,
         Trace'Access,
         Lexer.New_Lexer (Trace.Descriptor, Syntax),
         WisiToken.Generate.LR.LALR_Generate.Generate (Grammar, LALR_Descriptor, Grammar_File_Name => ""),
         User_Data                      => null,
         Language_Fixes                 => null,
         Language_Matching_Begin_Tokens => null,
         Language_String_ID_Set         => null);

      Execute_Command ("cdcd");
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("dragon_4_45_lalr_test.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_First'Access, "Test_First");
      Register_Routine (T, Test_LALR_Kernels'Access, "Test_LALR_Kernels");
      Register_Routine (T, Parser_Table'Access, "Parser_Table");
      Register_Routine (T, Test_Parse'Access, "Test_Parse");
   end Register_Tests;

   overriding procedure Tear_Down_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      WisiToken.Parse.LR.AUnit.Strict := True;
   end Tear_Down_Case;

end Dragon_4_45_LALR_Test;
