--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017 - 2020 Stephen Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Checks.Containers;
with GNAT.Source_Info;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Generate.LR1_Items;
with WisiToken.Productions;
package body Test_LR1_Parallel is

   ----------
   --  Test procedures

   procedure Run_Test (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      use AUnit.Checks;
      use AUnit.Checks.Containers;
      use WisiToken.Generate;
      use WisiToken.Generate.LR;
      use WisiToken;

      Test : Test_Case renames Test_Case (T);

      Grammar_File_Name : constant String := "../test/bnf/" & Test.Root_Name.all & ".wy";

      Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
        WisiToken.BNF.Generate_Utils.Parse_Grammar_File
          (Grammar_File_Name, WisiToken.BNF.LR1, WisiToken.BNF.re2c_Lexer, Ignore_Conflicts => True);
      --  Builds Generate_Data.Descriptor, Generate_Data.Grammar

      Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;
      Grammar    : WisiToken.Productions.Prod_Arrays.Vector renames Generate_Data.Grammar;

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Item_Sets_1 : constant LR1_Items.Item_Set_List := LR1_Generate.LR1_Item_Sets_Single
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

      Item_Sets_8_Array : constant LR1_Items.Item_Set_List := LR1_Generate.LR1_Item_Sets_Parallel
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor, Task_Count => 8);

      Item_Sets_8_Tree : LR1_Items.Item_Set_Tree;

      Map    : array (State_Index range Item_Sets_1.First_Index .. Item_Sets_1.Last_Index) of State_Index;
      Mapped : array (State_Index range Item_Sets_1.First_Index .. Item_Sets_1.Last_Index) of Boolean :=
        (others => False);

      use LR1_Items.Item_Set_Arrays;
      Found : Boolean;
   begin
      Check ("item_set.length", Item_Sets_8_Array.Length, Item_Sets_1.Length);

      for I in Map'Range loop
         Map (I) := Item_Sets_8_Tree.Find_Or_Insert (Item_Sets_8_Array (I).Tree_Node, Found).State;
         Mapped (Map (I)) := True;
      end loop;

      Check ("all states found", (for all M of Mapped => M), True);
   end Run_Test;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Run_test'Access, "Run_Test");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'(GNAT.Source_Info.File & " " & T.Root_Name.all);
   end Name;

end Test_LR1_Parallel;
