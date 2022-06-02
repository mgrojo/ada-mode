--  Abstract:
--
--  See spec
--
--  Copyright (C) 2013, 2015, 2017 - 2022 Stephen Leake
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

with AUnit.Assertions;
with AUnit.Checks.Containers;
with AUnit.Checks.Text_IO;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Source_Info;
with SAL;
with WisiToken.AUnit;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Generate.LR1_Items;
with WisiToken.Productions;
with WisiToken_Grammar_Runtime;
with WisiToken.Test_Util;
with WisiToken.Text_IO_Trace;
package body Test_LR1_Parallel is

   type State_Map is array (WisiToken.State_Index range <>) of WisiToken.State_Index;

   Trace_Generate_Table : Integer;

   procedure Check
     (Label    : in String;
      Map      : in State_Map;
      Computed : in WisiToken.Generate.LR1_Items.Goto_Item_List;
      Expected : in WisiToken.Generate.LR1_Items.Goto_Item_List)
   --  We can't use WisiToken.Generate.LR1_Items.AUnit for this; we need
   --  to map the states. Computed is from --task_count = 8, expected
   --  from --task_count = 1.
   is
      use AUnit.Checks;
      use WisiToken.AUnit;
      use WisiToken.Generate.LR1_Items.Goto_Item_Lists;
      Computed_Iter : constant Iterator := Computed.Iterate;
      Expected_Iter : constant Iterator := Expected.Iterate;

      Computed_I : Cursor  := Computed_Iter.First;
      Expected_I : Cursor  := Expected_Iter.First;
      Index      : Integer := 1;
   begin
      if Has_Element (Computed_I) or Has_Element (Expected_I) then
         Standard.AUnit.Assertions.Assert (Has_Element (Computed_I), Label & " Computed is empty");
         Standard.AUnit.Assertions.Assert (Has_Element (Expected_I), Label & " Expected is empty");
      else
         --  both are empty
         return;
      end if;

      loop
         Check (Label & Integer'Image (Index) & ".Symbol",
                Computed (Computed_I).Symbol, Expected (Expected_I).Symbol);
         Check (Label & Integer'Image (Index) & ".State",
                Computed (Computed_I).State,
                Map (Expected (Expected_I).State));
         Check (Label & Index'Image  & ".Next = null",
                not Has_Element (Computed_Iter.Next (Computed_I)),
                not Has_Element (Expected_Iter.Next (Expected_I)));
         Computed_I := Computed_Iter.Next (Computed_I);
         Expected_I := Expected_Iter.Next (Expected_I);
         Index      := Index + 1;
         exit when not Has_Element (Computed_I);
      end loop;
   end Check;

   procedure Put_Sets
     (File_Name  : in String;
      Item_Sets  : in WisiToken.Generate.LR1_Items.Item_Set_List;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor)
   is
      use Ada.Text_IO;
      use WisiToken;
      Parse_Table_File : File_Type;
   begin
      --  Output enough to make wisitoken-parse_table-mode work
      Create (Parse_Table_File, Out_File, File_Name);
      Set_Output (Parse_Table_File);
      Put_Line ("Productions:");
      New_Line;
      Put_Line ("LR1 Parse Table:");

      for State_Index in Item_Sets.First_Index .. Item_Sets.Last_Index loop
         Put_Line ("State" & State_Index'Image & ":");

         declare
            use WisiToken.Generate.LR1_Items;
         begin
            for Item of Item_Sets (State_Index).Set loop
               if In_Kernel (Grammar, Descriptor, Item) then
                  Put ("  " & Image (Grammar, Descriptor, Item, Show_Lookaheads => False));
                  New_Line;
               end if;
            end loop;
         end;
         New_Line;
         for Goto_Item of Item_Sets (State_Index).Goto_List loop
            Put_Line
              (Image (Goto_Item.Symbol, Descriptor) &
                 (if Goto_Item.Symbol in Descriptor.First_Terminal .. Descriptor.Last_Terminal
                  then " => goto state"
                  else "    goto state") &
                 Goto_Item.State'Image);
         end loop;
         New_Line;
      end loop;
      Set_Output (Standard_Output);
      Close (Parse_Table_File);
   end Put_Sets;

   ----------
   --  Test procedures

   procedure Compare_LR1_Sets (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Saved_Debug_Mode : constant Boolean := WisiToken.Debug_Mode;
   begin
      WisiToken.Debug_Mode := False;  -- EBNF tree edit leaves byte region out of order.
      declare
         use AUnit.Checks;
         use AUnit.Checks.Containers;
         use WisiToken.Generate;
         use WisiToken.Generate.LR;
         use WisiToken;

         Test : Test_Case renames Test_Case (T);

         Grammar_File_Name : constant String := "../test/bnf/" & Test.Root_Name.all & ".wy";

         Trace : aliased WisiToken.Text_IO_Trace.Trace;
         Input_Data : aliased WisiToken_Grammar_Runtime.User_Data_Type;
         Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
           WisiToken.BNF.Generate_Utils.Parse_Grammar_File
             (Grammar_File_Name, Input_Data'Unchecked_Access, WisiToken.BNF.LR1, WisiToken.BNF.re2c_Lexer,
              Trace'Unchecked_Access,
              Ignore_Conflicts => True);
         --  Builds Generate_Data.Descriptor, Generate_Data.Grammar
      begin
         WisiToken.Debug_Mode := Saved_Debug_Mode;
         declare
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
         begin
            WisiToken.Trace_Generate_Table := Test_LR1_Parallel.Trace_Generate_Table;
            declare
               Item_Sets_8_Array : constant LR1_Items.Item_Set_List := LR1_Generate.LR1_Item_Sets_Parallel
                 (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor, Task_Count => 8);

               Item_Sets_8_Tree : LR1_Items.Item_Set_Tree;

               Map    : State_Map (Item_Sets_1.First_Index .. Item_Sets_1.Last_Index);
               --  Map (t1_state) = t8_state
               Mapped : array (State_Index range Item_Sets_1.First_Index .. Item_Sets_1.Last_Index) of Boolean :=
                 (others => False);

               use LR1_Items.Item_Set_Arrays;
               Found : Boolean;
            begin
               if WisiToken.Trace_Action > Outline then
                  --  We can't use the lr1_t8_re2c.parse_table created by rules.make;
                  --  the state numbers change randomly with each run.
                  Put_Sets (Test.Root_Name.all & "_lr1_t8.set_table", Item_Sets_8_Array, Grammar, Descriptor);
               end if;

               Check ("item_set.length", Item_Sets_8_Array.Length, Item_Sets_1.Length);

               for Item_Set of Item_Sets_8_Array loop
                  Item_Sets_8_Tree.Insert (Item_Set.Tree_Node, Duplicate => SAL.Error);
               end loop;

               for I in Map'Range loop
                  Map (I) := Item_Sets_8_Tree.Find_Or_Insert (Item_Sets_1 (I).Tree_Node, Found).State;
                  Check (I'Image & ".found", Found, True);
                  Mapped (Map (I)) := True;
               end loop;

               Check ("all states found", (for all M of Mapped => M), True);

               for I in Map'Range loop
                  Check (I'Image & "-" & Map (I)'Image & ".goto_list",
                         Map,
                         Item_Sets_8_Array (Map (I)).Goto_List,
                         Item_Sets_1 (I).Goto_List);
               end loop;
            end;
         end;
      end;
   end Compare_LR1_Sets;

   procedure Compare_Parse (T : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (T);

      use AUnit.Checks;

      Input_Name : constant String := "../test/bnf/" & Test.Root_Name.all & ".input";

      Base_Name : constant String := Test.Root_Name.all & "_lr1";

      Exe_t1 : constant String := "./" & Base_Name & "_t1_run.exe";
      Exe_t8 : constant String := "./" & Base_Name & "_t8_run.exe";

      Output_t1 : constant String := Base_Name & "_t1-no_states.parse";
      Output_t8 : constant String := Base_Name & "_t8-no_states.parse";

      Args : constant GNAT.OS_Lib.String_List (1 .. 4) :=
        (1 => new String'("--verbosity"),
         2 => new String'("parse=22"),
         3 => new String'("-no-state-numbers"),
         4 => new String'(Input_Name));
   begin
      Check ("input file exists", Ada.Directories.Exists (Input_Name), True);

      WisiToken.Test_Util.Spawn (Exe_t1, Args, Output_t1);
      WisiToken.Test_Util.Dos2unix (Output_t1);

      WisiToken.Test_Util.Spawn (Exe_t8, Args, Output_t8);
      WisiToken.Test_Util.Dos2unix (Output_t8);

      AUnit.Checks.Text_IO.Check_Files ("", Output_t1, Output_t8);
   end Compare_Parse;

   ----------
   --  Public subprograms

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Compare_LR1_Sets'Access, "Compare_LR1_Sets");
      Register_Routine (T, Compare_Parse'Access, "Compare_Parse");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is begin
      return new String'(GNAT.Source_Info.File & " " & T.Root_Name.all);
   end Name;

   overriding procedure Set_Up_Case (T : in out Test_Case)
   is
      pragma Unreferenced (T);
   begin
      Trace_Generate_Table := WisiToken.Trace_Generate_Table;
      WisiToken.Trace_Generate_Table := 0;
   end Set_Up_Case;

end Test_LR1_Parallel;
