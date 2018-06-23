--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2013-2015, 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with WisiToken.Gen_Token_Enum;
with WisiToken.Lexer.Regexp;
with WisiToken.LR.LR1_Generator;
with WisiToken.LR.LR1_Items;
with WisiToken.LR.Parser;
with WisiToken.Productions;
with WisiToken.Syntax_Trees;
with WisiToken.Text_IO_Trace;
with WisiToken.Wisi_Ada; use WisiToken.Wisi_Ada;
with WisiToken.LR.LR1_Items.AUnit;
package body Grune_9_30 is

   type Token_ID is
     (
      --  terminals
      Lower_A_ID,
      Lower_B_ID,
      Lower_C_ID,
      EOF_ID,

      --  non-terminals
      Upper_S_ID,
      Upper_A_ID,
      Upper_B_ID
     );

   package Token_Enum is new WisiToken.Gen_Token_Enum
     (Token_Enum_ID     => Token_ID,
      First_Terminal    => Lower_A_ID,
      Last_Terminal     => EOF_ID,
      First_Nonterminal => Upper_S_ID,
      Last_Nonterminal  => Upper_B_ID,
      EOF_ID            => EOF_ID,
      Accept_ID         => Upper_S_ID,
      Case_Insensitive  => False);
   use Token_Enum;

   First_State_Index  : constant := 1;
   First_Parser_Label : constant := 1;

   Null_Action : WisiToken.Syntax_Trees.Semantic_Action renames WisiToken.Syntax_Trees.Null_Action;

   Grammar : constant WisiToken.Productions.Prod_Arrays.Vector :=
     Upper_S_ID <= Upper_A_ID & Upper_B_ID & Lower_C_ID & EOF_ID + Null_Action -- 1
     and
     Upper_A_ID <= Lower_A_ID + Null_Action                           -- 2
     and
     (Upper_B_ID <= Lower_B_ID + Null_Action                           -- 3
      or
                   +Null_Action)                                       -- 4
   ;

   package Lexer renames WisiToken.Lexer.Regexp;

   Syntax : constant Lexer.Syntax := To_Syntax
     ((
       Lower_A_ID => Lexer.Get ("a"),
       Lower_B_ID => Lexer.Get ("b"),
       Lower_C_ID => Lexer.Get ("c"),
       EOF_ID     => Lexer.Get ("" & Ada.Characters.Latin_1.EOT)
      ));

   Has_Empty_Production : constant WisiToken.Token_ID_Set :=
     WisiToken.LR.LR1_Items.Has_Empty_Production (Grammar, LR1_Descriptor);

   First : constant WisiToken.Token_Array_Token_Set := WisiToken.LR.LR1_Items.First
     (Grammar, LR1_Descriptor, Has_Empty_Production, Trace => False);

   Trace : aliased WisiToken.Text_IO_Trace.Trace (LR1_Descriptor'Access);

   ----------
   --  Test procedures

   procedure Test_Item_Sets (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (T);
      use Ada.Text_IO;
      use WisiToken.LR.LR1_Items.AUnit;
      use WisiToken.LR.LR1_Items;

      Computed : Item_Set_List := WisiToken.LR.LR1_Generator.LR1_Item_Sets
        (Has_Empty_Production, First, Grammar, First_State_Index, LR1_Descriptor,
         Trace => WisiToken.Trace_Generate > 0);

      Expected : Item_Set_List :=
        --  Item sets from [Grune] fig 9.31 a. States are numbered as in
        --  [Grune], but added to the list in the order we compute.
        --
        --  Our Item_Sets also include the gotos.
        (1 + (Get_Item (Grammar, (+Upper_S_ID, 0), 1, +EOF_ID) &
                Get_Item (Grammar, (+Upper_A_ID, 0), 1, +(Lower_B_ID, Lower_C_ID)))) &
        (2 + Get_Item (Grammar, (+Upper_A_ID, 0), 2, +(Lower_B_ID, Lower_C_ID))) &
        (3 + (Get_Item (Grammar, (+Upper_S_ID, 0), 2, +EOF_ID) &
                Get_Item (Grammar, (+Upper_B_ID, 0), 1, +Lower_C_ID) &
                Get_Item (Grammar, (+Upper_B_ID, 1), 1, +Lower_C_ID))) &
        (4 + Get_Item (Grammar, (+Upper_B_ID, 0), 2, +Lower_C_ID)) &
        (5 + Get_Item (Grammar, (+Upper_S_ID, 0), 3, +EOF_ID)) &
        (6 + Get_Item (Grammar, (+Upper_S_ID, 0), 4, +EOF_ID));

   begin
      Add_Gotos (Expected, 1, +(+Lower_A_ID, Get_Set (2, Expected)) & (+Upper_A_ID, Get_Set (3, Expected)));
      --  no gotos from state 2
      Add_Gotos (Expected, 3, +(+Lower_B_ID, Get_Set (4, Expected)) & (+Upper_B_ID, Get_Set (5, Expected)));
      --  no gotos from state 4
      Add_Gotos (Expected, 5, +(+Lower_C_ID, Get_Set (6, Expected)));

      if WisiToken.Trace_Generate > 0 then
         Put_Line ("computed:");
         Put (Grammar, LR1_Descriptor, Computed);
         Put_Line ("expected:");
         Put (Grammar, LR1_Descriptor, Expected);
      end if;

      Check ("", Computed, Expected);

      Free (Computed);
      Free (Expected);
   end Test_Item_Sets;

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
         WisiToken.LR.LR1_Generator.Generate (Grammar, LR1_Descriptor, First_State_Index),

         User_Data                    => null,
         Language_Fixes               => null,
         Language_Constrain_Terminals => null,
         Language_String_ID_Set       => null,
         First_Parser_Label           => First_Parser_Label);

      Execute_Command ("abc");
      Execute_Command ("ac");
   end Test_Parse;

   ----------
   --  Public subprograms

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("grune_9_30.adb");
   end Name;

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Item_Sets'Access, "Test_Item_Sets");
      Register_Routine (T, Test_Parse'Access, "Test_Parse");
   end Register_Tests;

end Grune_9_30;
--  Local_Variables:
--  eval: (ada-indent-opentoken-mode)
--  End:
