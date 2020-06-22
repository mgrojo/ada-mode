--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 - 2020 Free Software Foundation, Inc.
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Calendar;
with Ada.Containers;
with Ada.Exceptions;
with Ada.Text_IO;
with WisiToken.Generate;
package body WisiToken.Generate.LR.LR1_Generate is

   function LR1_Goto_Transitions
     (Set                     : in LR1_Items.Item_Set;
      Symbol                  : in Token_ID;
      Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor)
     return LR1_Items.Item_Set
   --  'goto' from [dragon] algorithm 4.9
   is
      use all type Ada.Containers.Count_Type;
      use Token_ID_Arrays;
      use LR1_Items;

      Goto_Set : Item_Set;
   begin
      for Item of Set.Set loop
         if Item.Dot /= No_Index then
            declare
               Dot : constant Token_ID_Arrays.Cursor := Productions.Constant_Ref_RHS
                 (Grammar, Item.Prod).Tokens.To_Cursor (Item.Dot);
            begin
               if Element (Dot) = Symbol and
                 --  We don't need a state with dot after EOI in the
                 --  accept production. EOI should only appear in the
                 --  accept production.
                 Symbol /= Descriptor.EOI_ID
               then
                  Goto_Set.Set.Insert ((Item.Prod, To_Index (Next (Dot)), Item.Lookaheads));
               end if;
            end;
         end if;
      end loop;

      if Goto_Set.Set.Length > 0 then
         return Closure (Goto_Set, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
      else
         return Goto_Set;
      end if;
   end LR1_Goto_Transitions;

   function LR1_Item_Sets
     (Has_Empty_Production    : in Token_ID_Set;
      First_Terminal_Sequence : in Token_Sequence_Arrays.Vector;
      Grammar                 : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor              : in WisiToken.Descriptor;
      Task_Count              : in System.Multiprocessors.CPU_Range;
      State_Limit             : in Unknown_State_Index := 0)
     return LR1_Items.Item_Set_List
   is
      use LR1_Items;
      use all type Ada.Containers.Count_Type;
      use all type SAL.Base_Peek_Type;
      use all type System.Multiprocessors.CPU_Range;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure
      --  "items", with some optimizations.

      protected Supervisor is

         procedure Initialize (First_Item_Set : in Item_Set);

         entry Get
           (Worker_ID     : in     Integer;
            Set           :    out Item_Set;
            Worker_C_Tree : in out Item_Set_Trees.Tree);
         --  Get a new state to check. Available when there is a state to
         --  check, or when all states have been checked and all workers are
         --  inactive; then Set.State is Invalid_State_Index.
         --
         --  If returning a valid state, add new sets from other workers to
         --  C_Tree, and increment active worker count.

         procedure Update
           (Worker_ID           : in     Integer;
            From_State          : in     State_Index;
            New_C               : in out Item_Set_List;
            New_C_Tree          : in     Item_Set_Trees.Tree;
            Existing_Goto_Items : in     Goto_Item_Arrays.Vector;
            New_Goto_Items      : in out Goto_Item_Arrays.Vector;
            Worker_C_Tree       : in out Item_Set_Trees.Tree);
         --  Add New_Item_Sets (may be empty) to C.
         --  Add New_Goto_Items (may be empty) to C (State).Goto_List.
         --
         --  Decrement active worker count.

         procedure Fatal_Error
           (Exception_ID : in Ada.Exceptions.Exception_Id;
            Message      : in String);
         --  Worker encountered an exception; record it for Done, decrement
         --  active worker count.

         entry Done
           (ID      : out Ada.Exceptions.Exception_Id;
            Message : out Ada.Strings.Unbounded.Unbounded_String);
         --  Available when all states have been checked, and all workers
         --  inactive.

         function Get_C return Item_Set_List;

      private
         C               : Item_Set_List;       -- result
         C_Tree          : Item_Set_Trees.Tree; -- for fast find
         States_To_Check : State_Index_Queues.Queue;
         --  [dragon] specifies 'until no more items can be added', but we use
         --  a queue to avoid checking unecessary states. Ada LR1 has over
         --  100,000 states, so this is a significant gain (reduced time from
         --  600 seconds to 40).

         Active_Workers : Natural := 0;
         Fatal          : Boolean := False;

         Error_ID       : Ada.Exceptions.Exception_Id := Ada.Exceptions.Null_Id;
         Error_Message  : Ada.Strings.Unbounded.Unbounded_String;

         Summary_Last_Output : State_Index := 0;
      end Supervisor;

      protected body Supervisor is

         procedure Initialize (First_Item_Set : in Item_Set)
         is
            First_State_Index : constant State_Index := First_Item_Set.State;
         begin
            C_Tree.Trace_Iterators (WisiToken.Trace_Generate_Table > Detail);
            if Debug_Mode then
               C_Tree.Control_Iterators (Item_Set_Trees.Single);
            end if;

            C.Set_First_Last (First_State_Index, First_State_Index - 1);

            Add (Grammar, First_Item_Set, C, C_Tree, Descriptor, Include_Lookaheads => True);

            States_To_Check.Put (First_State_Index);
         end Initialize;

         entry Get
           (Worker_ID     : in     Integer;
            Set           :    out Item_Set;
            Worker_C_Tree : in out Item_Set_Trees.Tree)
           when Fatal or States_To_Check.Length > 0 or Active_Workers = 0
         is
            New_States : Item_Set_Tree_Node_Arrays.Vector; -- Outside scope of Worker_Iter
         begin
            if States_To_Check.Length > 0 then
               Set := C (States_To_Check.Get);

               if Trace_Generate_Table > Detail then
                  Ada.Text_IO.Put ("(worker" & Worker_ID'Image & ") Checking ");
                  Put (Grammar, Descriptor, Set, Show_Lookaheads => True, Show_Goto_List => True);
               end if;

               --  FIXME: cache New_C from other workers, to avoid traversing
               --  the whole tree
               declare
                  use Item_Set_Trees;
                  use Int_Arrays_Comparable;
                  use all type SAL.Compare_Result;

                  Super_Iter  : constant Item_Set_Trees.Iterator := Supervisor.C_Tree.Iterate;
                  Worker_Iter : constant Item_Set_Trees.Iterator := Worker_C_Tree.Iterate;

                  Super_Cur  : Item_Set_Trees.Cursor := Super_Iter.First;
                  Worker_Cur : Item_Set_Trees.Cursor := Worker_Iter.First;
               begin
                  loop
                     exit when not Has_Element (Super_Cur);
                     exit when not Has_Element (Worker_Cur);
                     if Compare (Key (Super_Cur), Key (Worker_Cur)) /= Equal then
                        New_States.Append (Supervisor.C_Tree.Constant_Ref (Super_Cur));
                     end if;
                     Super_Cur  := Super_Iter.Next (@);
                     Worker_Cur := Worker_Iter.Next (@);
                  end loop;

                  if Has_Element (Super_Cur) and not Has_Element (Worker_Cur) then
                     loop
                        New_States.Append (Supervisor.C_Tree.Constant_Ref (Super_Cur));
                        Super_Cur  := Super_Iter.Next (@);
                        exit when not Has_Element (Super_Cur);
                     end loop;
                  end if;
               end;
               for Item of New_States loop
                  Worker_C_Tree.Insert (Item);
               end loop;

               Active_Workers := @ + 1;
            else
               Set := (others => <>);
            end if;

            if Trace_Generate_Table > Outline and then
              C.Last_Index > Summary_Last_Output + 500
            then
               Ada.Text_IO.Put_Line
                 ("(super) states:" & C.Last_Index'Image & " States_To_Check:" & States_To_Check.Length'Image);
               Summary_Last_Output := C.Last_Index;
            end if;
         end Get;

         procedure Update
           (Worker_ID           : in     Integer;
            From_State          : in     State_Index;
            New_C               : in out Item_Set_List;
            New_C_Tree          : in     Item_Set_Trees.Tree;
            Existing_Goto_Items : in     Goto_Item_Arrays.Vector;
            New_Goto_Items      : in out Goto_Item_Arrays.Vector;
            Worker_C_Tree       : in out Item_Set_Trees.Tree)
         is
         begin
            declare
               use Goto_Item_Lists;
               From_Goto_List : Goto_Item_List renames C (From_State).Goto_List;
            begin
               for Item of Existing_Goto_Items loop
                  if not Has_Element (From_Goto_List.Iterate.Find (Item.Symbol)) then
                     From_Goto_List.Insert (Item);
                     if Trace_Generate_Table > Detail then
                        Ada.Text_IO.Put_Line
                          ("(worker" & Worker_ID'Image & ") state" & From_State'Image & " adding goto on " &
                             Image (Item.Symbol, Descriptor) & " to state" & Item.State'Image);
                     end if;
                  end if;
               end loop;
            end;

            if Trace_Generate_Table > Detail and then New_C.Length > 0 then
               Ada.Text_IO.Put_Line
                 ("(worker" & Worker_ID'Image & ") adding" & New_C.Length'Image & " states from state" &
                    From_State'Image);
            end if;
            for New_C_Node of New_C_Tree loop
               declare
                  use Item_Set_Trees;

                  From_Goto_List : Goto_Item_List renames C (From_State).Goto_List;

                  Found_Cur : constant Cursor := Supervisor.C_Tree.Iterate.Find (New_C_Node.Key);

                  New_State    : constant State_Index         := C.Last_Index + 1;
                  Worker_State : constant Positive_Index_Type := Positive_Index_Type (New_C_Node.State);
               begin
                  if Has_Element (Found_Cur) then
                     New_Goto_Items (Worker_State).State := Supervisor.C_Tree.Constant_Ref (Found_Cur).State;
                     From_Goto_List.Insert (New_Goto_Items (Worker_State));

                  else
                     New_Goto_Items (Worker_State).State := New_State;
                     From_Goto_List.Insert (New_Goto_Items (Worker_State));

                     New_C (New_C_Node.State).State := New_State;

                     States_To_Check.Put (New_State);

                     Add (Grammar, New_C (New_C_Node.State), New_C_Node.Key, Supervisor.C, Supervisor.C_Tree,
                          Descriptor, Worker_C_Tree);

                     if Trace_Generate_Table > Detail then
                        Put (Grammar, Descriptor, New_C (New_C_Node.State),
                             Show_Lookaheads => True,
                             Show_Goto_List  => True);
                     end if;
                  end if;

                  if Trace_Generate_Table > Detail then
                     Ada.Text_IO.Put_Line
                       ("    state" & From_State'Image & " adding goto on " &
                          Image (New_Goto_Items (Worker_State).Symbol, Descriptor) & " to state" &
                          New_Goto_Items (Worker_State).State'Image);
                  end if;
               end;
            end loop;

            if State_Limit /= 0 and then State_Limit < C.Last_Index then
               raise SAL.Programmer_Error with "state_limit exceeded";
            end if;

            Active_Workers := @ - 1;
         exception
         when E : others =>

            Active_Workers := @ - 1;
            Fatal          := True;
            States_To_Check.Clear; -- force an early end.
            declare
               use Ada.Text_IO;
               use Ada.Exceptions;
            begin
               Error_ID       := Exception_Identity (E);
               Error_Message  := +Exception_Message (E);
               Put_Line
                 (Standard_Error, "(super) Update exception: " & Exception_Name (E) & ": " & Exception_Message (E));
            end;
         end Update;

         procedure Fatal_Error
           (Exception_ID : in Ada.Exceptions.Exception_Id;
            Message      : in String)
         is begin
            Supervisor.Error_ID      := Exception_ID;
            Supervisor.Error_Message := +Message;

            States_To_Check.Clear; -- force an early end.
            Fatal          := True;
            Active_Workers := @ - 1;
         end Fatal_Error;

         entry Done
           (ID      : out Ada.Exceptions.Exception_Id;
            Message : out Ada.Strings.Unbounded.Unbounded_String)
           when Fatal or (Active_Workers = 0 and States_To_Check.Is_Empty)
         is begin
            ID      := Supervisor.Error_ID;
            Message := Supervisor.Error_Message;
         end Done;

         function Get_C return Item_Set_List
         is begin
            return C;
         end Get_C;

      end Supervisor;

      task type Worker_Task
      is
         entry Start (ID : in Integer);
         --  Start states from Supervisor. Stop when Supervisor returns
         --  Invalid_State_Index;
      end Worker_Task;

      task body Worker_Task
      is
         ID : Integer;

         C_Tree : Item_Set_Trees.Tree; -- Local copy for fast find
         C_I    : Item_Set;            --  C (I) from Supervisor

         procedure Check_State
         is
            New_C : Item_Set_Arrays.Vector;
            --  New states, numbered from 1 (the local new state number); add that
            --  to the real state index.

            New_C_Tree : Item_Set_Trees.Tree;
            --  For searching

            Existing_Goto_List : Goto_Item_Arrays.Vector;
            --  From C_I.state to an existing state (found in C_Tree).

            New_Goto_Items : Goto_Item_Arrays.Vector;
            --  For each new state, there is one Goto from C_I to the new state.
            --  It is stored in New_Goto_Items, which is indexed by the local new
            --  state number. This simplifies finding the actual state number in
            --  Supervisor.C_Tree; see Supervisor.Update.

            New_State : State_Index := 1;
         begin
            New_Goto_Items.Set_First_Last
              (First => Positive_Index_Type (New_State),
               Last  => Positive_Index_Type (New_State) - 1);

            New_C.Set_First_Last (First => New_State, Last => New_State - 1);

            for Dot_ID_I in C_I.Dot_IDs.First_Index .. C_I.Dot_IDs.Last_Index loop
               --  [dragon] has 'for each grammar symbol X', but LR1_Goto_Transitions
               --  rejects Symbol that is not in Dot_IDs, so we iterate over that.

               declare
                  Symbol       : Token_ID renames C_I.Dot_IDs (Dot_ID_I);
                  New_Item_Set : Item_Set := LR1_Goto_Transitions
                    (C_I, Symbol, Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);

               begin
                  if New_Item_Set.Set.Length > 0 then -- 'goto (I, X) not empty'
                     declare
                        use Item_Set_Trees;
                        New_Item_Set_Key : constant Item_Set_Tree_Key := To_Item_Set_Tree_Key
                          (New_Item_Set, Descriptor, True);

                        --  First search in Worker.C_Tree
                        Found_Cur : Cursor := C_Tree.Iterate.Find (New_Item_Set_Key);

                        Found_State : constant Unknown_State_Index :=
                          (if Has_Element (Found_Cur)
                           then C_Tree.Constant_Ref (Found_Cur).State
                           else Unknown_State);
                     begin
                        if Found_State = Unknown_State then
                           Found_Cur := New_C_Tree.Iterate.Find (New_Item_Set_Key);

                           if Has_Element (Found_Cur) then
                              --  There is already a goto from C_I to New_State.
                              null;
                           else
                              New_Goto_Items.Append ((Symbol, New_State));
                              pragma Assert (New_State = State_Index (New_Goto_Items.Last_Index));

                              New_Item_Set.State := New_State;
                              New_C.Append (New_Item_Set);
                              pragma Assert (New_State = State_Index (New_C.Last_Index));

                              New_C_Tree.Insert ((New_Item_Set_Key, New_State));

                              New_State := New_State + 1;
                           end if;
                        else
                           if not Is_In ((Symbol, Found_State), Goto_List => C_I.Goto_List) then
                              Existing_Goto_List.Append ((Symbol, Found_State));
                           end if;
                        end if;
                     end;
                  end if;
               end;
            end loop;

            Supervisor.Update (ID, C_I.State, New_C, New_C_Tree, Existing_Goto_List, New_Goto_Items, C_Tree);
         end Check_State;
      begin
         select
            accept Start (ID : in Integer)

            do
               Worker_Task.ID := ID;
            end Start;
         or
            terminate;
         end select;

         loop
            Supervisor.Get (ID, C_I, C_Tree);
            exit when C_I.State = Unknown_State;
            Check_State;
         end loop;

         if Trace_Generate_Table > Outline then
            Ada.Text_IO.Put_Line ("(worker" & ID'Image & ") terminate");
         end if;
      exception
      when E : others =>
         Supervisor.Fatal_Error (Ada.Exceptions.Exception_Identity (E), Ada.Exceptions.Exception_Message (E));
         if Trace_Generate_Table > Outline then
            Ada.Text_IO.Put_Line ("(worker" & ID'Image & ") terminate on exception");
         end if;
      end Worker_Task;

      Worker_Tasks : array
        (1 .. System.Multiprocessors.CPU_Range'Min
           (Task_Count,
            System.Multiprocessors.CPU_Range'Max (1, System.Multiprocessors.Number_Of_CPUs)))
        of Worker_Task;

      First_State_Index : constant State_Index := 0;

      First_Item_Set : constant Item_Set := Closure
        ((Set            => Item_Lists.To_List
            ((Prod       => (Grammar.First_Index, 0),
              Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First_Index,
              Lookaheads => To_Lookahead (Descriptor.EOI_ID))),
          Goto_List      => <>,
          Dot_IDs        => <>,
          State          => First_State_Index),
         Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor);
   begin
      Supervisor.Initialize (First_Item_Set);

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.Put_Line (Worker_Tasks'Length'Image & " lr1_items worker tasks");
      end if;

      for I in Worker_Tasks'Range loop
         Worker_Tasks (I).Start (Integer (I));
      end loop;

      declare
         use Ada.Exceptions;
         ID      : Exception_Id;
         Message : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Supervisor.Done (ID, Message); -- Wait for all states to be checked

         if ID /= Null_Id then
            for I in Worker_Tasks'Range loop
               if not Worker_Tasks (I)'Terminated then
                  abort Worker_Tasks (I);
               end if;
            end loop;
            Raise_Exception (ID, -Message);
         else
            if Trace_Generate_Table > Outline then
               Ada.Text_IO.Put_Line ("super reports done");
            end if;
         end if;
      end;
      return Supervisor.Get_C;
   end LR1_Item_Sets;

   procedure Add_Actions
     (Item_Sets  : in     LR1_Items.Item_Set_List;
      Table      : in out Parse_Table;
      Grammar    : in     WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in     WisiToken.Descriptor)
   is
      --  Add actions for all Item_Sets to Table.
   begin
      for Item_Set of Item_Sets loop
         Add_Actions (Item_Set, Table, Grammar, Descriptor);
      end loop;

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   function Generate
     (Grammar               : in out WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor            : in     WisiToken.Descriptor;
      Grammar_File_Name     : in     String;
      Known_Conflicts       : in     Conflict_Lists.Tree              := Conflict_Lists.Empty_Tree;
      McKenzie_Param        : in     McKenzie_Param_Type              := Default_McKenzie_Param;
      Parse_Table_File_Name : in     String                           := "";
      Include_Extra         : in     Boolean                          := False;
      Ignore_Conflicts      : in     Boolean                          := False;
      Partial_Recursion     : in     Boolean                          := True;
      Task_Count            : in     System.Multiprocessors.CPU_Range := 1)
     return Parse_Table_Ptr
   is
      Ignore_Unused_Tokens     : constant Boolean := WisiToken.Trace_Generate_Table > Detail;
      Ignore_Unknown_Conflicts : constant Boolean := Ignore_Conflicts or WisiToken.Trace_Generate_Table > Detail;
      Unused_Tokens            : constant Boolean := WisiToken.Generate.Check_Unused_Tokens (Descriptor, Grammar);

      Table : Parse_Table_Ptr;

      Nullable : constant Token_Array_Production_ID := WisiToken.Generate.Nullable (Grammar);
      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Nullable);

      Recursions : constant WisiToken.Generate.Recursions :=
        (if Partial_Recursion
         then WisiToken.Generate.Compute_Partial_Recursion (Grammar, Descriptor)
         else WisiToken.Generate.Compute_Full_Recursion (Grammar, Descriptor));
      Recursions_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      Minimal_Terminal_Sequences : constant Minimal_Sequence_Array :=
        Compute_Minimal_Terminal_Sequences (Descriptor, Grammar, Grammar_File_Name);

      Minimal_Terminal_First : constant Token_Array_Token_ID :=
        Compute_Minimal_Terminal_First (Descriptor, Minimal_Terminal_Sequences);

      First_Nonterm_Set : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      First_Terminal_Sequence : constant Token_Sequence_Arrays.Vector :=
        WisiToken.Generate.To_Terminal_Sequence_Array (First_Nonterm_Set, Descriptor);

      Item_Sets : constant LR1_Items.Item_Set_List := LR1_Item_Sets
        (Has_Empty_Production, First_Terminal_Sequence, Grammar, Descriptor, Task_Count);

      Conflict_Counts      : Conflict_Count_Lists.Vector;
      Unknown_Conflicts    : Conflict_Lists.Tree;
      Known_Conflicts_Edit : Conflict_Lists.Tree := Known_Conflicts;

      Initial_Item_Sets_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      Add_Actions_Time       : Ada.Calendar.Time;
      Minimal_Actions_Time   : Ada.Calendar.Time;
      Collect_Conflict_Time  : Ada.Calendar.Time;
   begin
      if Trace_Time then
         Ada.Text_IO.Put_Line
           ("initial item_sets time:" & Duration'Image (Ada.Calendar."-" (Initial_Item_Sets_Time, Recursions_Time)));
      end if;
      if Trace_Generate_Table + Trace_Generate_Minimal_Complete > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR1_Generate:");
         if Trace_Generate_Table > Outline then
            Ada.Text_IO.Put_Line ("Item_Sets:");
            LR1_Items.Put (Grammar, Descriptor, Item_Sets);
         end if;
         Ada.Text_IO.New_Line;
      end if;

      Table := new Parse_Table
        (State_First       => Item_Sets.First_Index,
         State_Last        => Item_Sets.Last_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      if McKenzie_Param = Default_McKenzie_Param then
         --  Descriminants in Default are wrong
         Table.McKenzie_Param :=
           (First_Terminal              => Descriptor.First_Terminal,
            Last_Terminal               => Descriptor.Last_Terminal,
            First_Nonterminal           => Descriptor.First_Nonterminal,
            Last_Nonterminal            => Descriptor.Last_Nonterminal,
            Insert                      => (others => 0),
            Delete                      => (others => 0),
            Push_Back                   => (others => 0),
            Undo_Reduce                 => (others => 0),
            Minimal_Complete_Cost_Delta => Default_McKenzie_Param.Minimal_Complete_Cost_Delta,
            Fast_Forward                => Default_McKenzie_Param.Fast_Forward,
            Matching_Begin              => Default_McKenzie_Param.Matching_Begin,
            Ignore_Check_Fail           => Default_McKenzie_Param.Ignore_Check_Fail,
            Task_Count                  => Default_McKenzie_Param.Task_Count,
            Check_Limit                 => Default_McKenzie_Param.Check_Limit,
            Check_Delta_Limit           => Default_McKenzie_Param.Check_Delta_Limit,
            Enqueue_Limit               => Default_McKenzie_Param.Enqueue_Limit);
      else
         Table.McKenzie_Param := McKenzie_Param;
      end if;

      Add_Actions (Item_Sets, Table.all, Grammar, Descriptor);

      if Trace_Time then
         Add_Actions_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("add_actions time:" & Duration'Image (Ada.Calendar."-" (Add_Actions_Time, Initial_Item_Sets_Time)));
      end if;

      for State in Table.States'Range loop
         if Trace_Generate_Minimal_Complete > Extra then
            Ada.Text_IO.Put_Line ("Set_Minimal_Complete_Actions:" & State_Index'Image (State));
         end if;
         WisiToken.Generate.LR.Set_Minimal_Complete_Actions
           (Table.States (State),
            LR1_Items.Filter (Item_Sets (State), Grammar, Descriptor, LR1_Items.In_Kernel'Access),
            Descriptor, Grammar, Nullable, Minimal_Terminal_Sequences, Minimal_Terminal_First);
      end loop;

      if Trace_Time then
         Minimal_Actions_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("compute minimal actions time:" & Duration'Image
              (Ada.Calendar."-" (Minimal_Actions_Time, Add_Actions_Time)));
      end if;

      Collect_Conflicts (Table.all, Unknown_Conflicts, Conflict_Counts);

      if Trace_Time then
         Collect_Conflict_Time := Ada.Calendar.Clock;
         Ada.Text_IO.Put_Line
           ("compute conflicts time:" & Duration'Image
              (Ada.Calendar."-" (Collect_Conflict_Time, Minimal_Actions_Time)));
      end if;

      if Parse_Table_File_Name /= "" then
         WisiToken.Generate.LR.Put_Parse_Table
           (Table, Parse_Table_File_Name, "LR1", Grammar, Recursions, Item_Sets, Conflict_Counts, Descriptor,
            Include_Extra);
      end if;

      if Trace_Generate_Table > Outline then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Has_Empty_Production: " & Image (Has_Empty_Production, Descriptor));

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Minimal_Terminal_First:");
         for ID in Minimal_Terminal_First'Range loop
            Ada.Text_IO.Put_Line
              (Image (ID, Descriptor) & " =>" &
                 (if Minimal_Terminal_First (ID) = Invalid_Token_ID
                  then ""
                  else ' ' & Image (Minimal_Terminal_First (ID), Descriptor)));
         end loop;
      end if;

      Check_Conflicts
        ("LR1", Unknown_Conflicts, Known_Conflicts_Edit, Grammar_File_Name, Descriptor, Ignore_Unknown_Conflicts);

      WisiToken.Generate.Error := WisiToken.Generate.Error or (Unused_Tokens and not Ignore_Unused_Tokens);

      return Table;
   end Generate;

end WisiToken.Generate.LR.LR1_Generate;
