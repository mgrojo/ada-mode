--  Abstract :
--
--  Show difference between lr1 task_count 1 and 0
--
--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Traceback.Symbolic;
with SAL;
with WisiToken.BNF.Generate_Utils;
with WisiToken.Generate.LR.LR1_Generate;
with WisiToken.Generate.LR1_Items; use WisiToken.Generate.LR1_Items;
with WisiToken.Productions;
procedure WisiToken.LR1_Parallel
is
   --  procedure Put_Usage
   --  is begin
   --     Put_Line ("wisitoken-lr1_parallel <grammar file> [verbosity]");
   --  end Put_Usage;
begin
   declare
      --  Catch exceptions
      Grammar_File_Name : constant String := Ada.Command_Line.Argument (1);

      Generate_Data : aliased WisiToken.BNF.Generate_Utils.Generate_Data :=
        WisiToken.BNF.Generate_Utils.Parse_Grammar_File
          (Grammar_File_Name,
           Generate_Algorithm => WisiToken.BNF.LR1,
           Lexer              => WisiToken.BNF.re2c_Lexer,
           Ignore_Conflicts   => True);
      --  Builds Generate_Data.Descriptor, Generate_Data.Grammar

      Descriptor : WisiToken.Descriptor renames Generate_Data.Descriptor.all;
      Grammar    : WisiToken.Productions.Prod_Arrays.Vector renames Generate_Data.Grammar;

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      Item_Sets_1 : constant Item_Set_List := WisiToken.Generate.LR.LR1_Generate.LR1_Item_Sets
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor, Task_Count => 1);

      Time_1 : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      --  We assume LR1_Item_Sets is correct with task_count = 1; tested in
      --  test_bnf_suite. So set debug flags for count = 8 case.

      Put_Line ("task_count 1 time:" & Duration'Image (Ada.Calendar."-" (Time_1, Time_Start)));
      Put_Line ("task_count 1 max state:" & Item_Sets_1.Last_Index'Image);

      WisiToken.Trace_Generate_Table :=
        (if Ada.Command_Line.Argument_Count > 1
         then Integer'Value (Ada.Command_Line.Argument (2))
         else 0);

      WisiToken.Debug_Mode := True;

      declare
         Item_Sets_8_Array : constant Item_Set_List := WisiToken.Generate.LR.LR1_Generate.LR1_Item_Sets
           (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor,
            Task_Count  => 8,
            State_Limit => Item_Sets_1.Last_Index * 2);

         Time_8 : constant Ada.Calendar.Time := Ada.Calendar.Clock;

         Item_Sets_8_Tree : Item_Set_Trees.Tree;

         Map    : array (State_Index range Item_Sets_1.First_Index .. Item_Sets_1.Last_Index) of State_Index;
         Mapped : array (State_Index range Item_Sets_8_Array.First_Index .. Item_Sets_8_Array.Last_Index) of Boolean :=
           (others => False);

         use Item_Set_Arrays;
      begin
         Put_Line ("task_count 8 time:" & Duration'Image (Ada.Calendar."-" (Time_8, Time_1)));

         for I in Map'Range loop
            Item_Sets_8_Tree.Insert
              ((To_Item_Set_Tree_Key (Item_Sets_8_Array (I), Descriptor, Include_Lookaheads => True),
                I));
         end loop;

         for I in Map'Range loop
            begin
               Map (I) := Item_Sets_8_Tree.Constant_Ref
                 (To_Item_Set_Tree_Key (Item_Sets_1 (I), Descriptor, Include_Lookaheads => True)).State;
               Mapped (Map (I)) := True;
            exception
            when SAL.Not_Found =>
               Put_Line ("item_sets_1 state" & I'Image & " not found");
            end;
         end loop;

         Put ("extra states:");

         for I in Mapped'Range loop
            if not Mapped (I) then
               Put (I'Image);
            end if;
         end loop;
      end;
   end;
exception
when E : others =>
   declare
      use Ada.Exceptions;
      use Ada.Command_Line;
   begin
      Put_Line (Standard_Error, Exception_Name (E) & ": " & Exception_Message (E));
      Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Set_Exit_Status (Failure);
   end;
end WisiToken.LR1_Parallel;
