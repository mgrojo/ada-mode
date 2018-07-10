--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017, 2018 Stephe Leake
--  Copyright (C) 1999 Ted Dennison
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

with Ada.Containers;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with WisiToken.Generate;
package body WisiToken.LR.LALR_Generate is

   --  The following types are used for computing lookahead
   --  propagations.

   type Item_List;
   type Item_List_Ptr is access Item_List;
   type Item_List is record
      Item : LR1_Items.Item_Ptr;
      Next : Item_List_Ptr;
   end record;

   type Item_Item_List_Mapping;
   type Item_Item_List_Mapping_Ptr is access Item_Item_List_Mapping;

   type Item_Item_List_Mapping is record
      From : LR1_Items.Item_Ptr;
      To   : Item_List_Ptr;
      Next : Item_Item_List_Mapping_Ptr;
   end record;

   procedure Dispose is new Ada.Unchecked_Deallocation (Item_Item_List_Mapping, Item_Item_List_Mapping_Ptr);
   procedure Dispose is new Ada.Unchecked_Deallocation (Item_List, Item_List_Ptr);

   procedure Free (List : in out Item_List_Ptr)
   is
      Old_Item : Item_List_Ptr := List;
   begin
      while Old_Item /= null loop
         List := Old_Item.Next;
         Dispose (Old_Item);
         Old_Item := List;
      end loop;
   end Free;

   procedure Free (List : in out Item_Item_List_Mapping_Ptr)
   is
      Old_Mapping : Item_Item_List_Mapping_Ptr := List;
   begin
      while Old_Mapping /= null loop
         List := Old_Mapping.Next;
         Free (Old_Mapping.To);
         Dispose (Old_Mapping);
         Old_Mapping := List;
      end loop;
   end Free;

   function Propagate_Lookahead (Descriptor : in LALR_Descriptor) return Token_ID_Set
   is
      Result : Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Propagate_ID) := (others => False);
   begin
      Result (Descriptor.Propagate_ID) := True;
      return Result;
   end Propagate_Lookahead;

   function Null_Lookahead (Descriptor : in LALR_Descriptor) return Token_ID_Set
   is
      Result : constant Token_ID_Set (Descriptor.First_Terminal .. Descriptor.Propagate_ID) := (others => False);
   begin
      return Result;
   end Null_Lookahead;

   ----------
   --  Debug output

   procedure Put
     (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor   : in WisiToken.Descriptor'Class;
      Propagations : in Item_Item_List_Mapping_Ptr)
   is
      Next_Prop : Item_Item_List_Mapping_Ptr := Propagations;
      Next_To   : Item_List_Ptr;
   begin
      while Next_Prop /= null loop

         Ada.Text_IO.Put ("From ");
         LR1_Items.Put (Grammar, Descriptor, Next_Prop.From, Show_Lookaheads => True);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("To   ");
            LR1_Items.Put (Grammar, Descriptor, Next_To.Item, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;

            Next_To := Next_To.Next;
         end loop;

         Next_Prop := Next_Prop.Next;
      end loop;

   end Put;

   procedure Put_Parse_Table
     (Table      : in Parse_Table_Ptr;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Kernels    : in LR1_Items.Item_Set_List;
      Descriptor : in LALR_Descriptor)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Tokens:");
      WisiToken.Put_Tokens (Descriptor);
      New_Line;
      Put_Line ("Productions:");
      WisiToken.Productions.Put (Grammar, Descriptor);
      New_Line;

      Put_Line ("LALR Parse Table:");

      for State in Table.States'Range loop
         LR1_Items.Put (Grammar, Descriptor, Kernels (State), Show_Lookaheads => True);
         New_Line;
         Put (Descriptor, Table.States (State));

         if State /= Table.States'Last then
            New_Line;
         end if;
      end loop;

      if Table.McKenzie_Param.Cost_Limit /= WisiToken.LR.Default_McKenzie_Param.Cost_Limit then
         New_Line;
         Put_Line ("McKenzie:");
         WisiToken.LR.Put (Table.McKenzie_Param, Descriptor);
      end if;

      New_Line;
      Put_Line ("Minimal_Terminal_Sequences:");
      for I in Table.Minimal_Terminal_Sequences.First_Index ..
        Table.Minimal_Terminal_Sequences.Last_Index
      loop
         Put_Line
           (WisiToken.Image (I, Descriptor) & " => " & WisiToken.Image
              (Table.Minimal_Terminal_Sequences (I), Descriptor));
      end loop;
   end Put_Parse_Table;

   ----------
   --  Generate utils

   function LALR_Goto_Transitions
     (Kernel     : in LR1_Items.Item_Set;
      Symbol     : in Token_ID;
      First      : in Token_Array_Token_Set;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in LALR_Descriptor)
     return LR1_Items.Item_Set
   is
      use Token_ID_Arrays;
      use LR1_Items;

      Goto_Set : Item_Set;

      Item   : Item_Ptr := Kernel.Set;
      Dot_ID : Token_ID;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop

         if Dot (Item) /= No_Element then

            Dot_ID := Element (Dot (Item));
            --  ID of token after Dot

            --  If Symbol = EOF_Token, this is the start symbol accept
            --  production; don't need a kernel with dot after EOF.
            if Dot_ID = Symbol and
              Symbol /= Descriptor.EOF_ID and
              null = Find (Prod_ID (Item), Next (Dot (Item)), Goto_Set, Match_Lookaheads => False)
            then
               Add
                 (Goto_Set.Set,
                  New_Item_Node
                    (Prod       => Prod_ID (Item),
                     Dot        => Next (Dot (Item)),
                     State      => Unknown_State, -- replaced in Kernels
                     Lookaheads => Lookaheads (Item)));

               if Trace_Generate > Detail then
                  Ada.Text_IO.Put_Line ("LALR_Goto_Transitions " & Image (Symbol, Descriptor));
                  Put (Grammar, Descriptor, Goto_Set, Show_Lookaheads => True, Show_Goto_List => True);
               end if;
            end if;

            if Dot_ID in Descriptor.First_Nonterminal .. Descriptor.Last_Nonterminal and then
              First (Dot_ID, Symbol)
            then
               --  Find the production(s) that create Dot_ID
               --  with first token Symbol and put them in.
               --
               --  This is equivalent to Filter (LR1_Items.Closure,
               --  In_Kernel), but more efficient, because it does not
               --  generate non-kernel items. See Test/compare_goto_transitions.adb.
               for Prod of Grammar loop
                  for RHS_2_I in Prod.RHSs.First_Index .. Prod.RHSs.Last_Index loop
                     declare
                        P_ID  : constant Production_ID := (Prod.LHS, RHS_2_I);
                        Dot_2 : constant Cursor        := Prod.RHSs (RHS_2_I).Tokens.First;
                     begin
                        if (Dot_ID = Prod.LHS or First (Dot_ID, Prod.LHS)) and
                          (Dot_2 /= No_Element and then Element (Dot_2) = Symbol)
                        then
                           if null = Find (P_ID, Next (Dot_2), Goto_Set, Match_Lookaheads => False) then
                              Add
                                (Goto_Set.Set,
                                 New_Item_Node
                                   (Prod       => P_ID,
                                    Dot        => Next (Dot_2),
                                    State      => Unknown_State, -- replaced in Kernels
                                    Lookaheads => Null_Lookahead (Descriptor)));

                              if Trace_Generate > Detail then
                                 Ada.Text_IO.Put_Line ("LALR_Goto_Transitions " & Image (Symbol, Descriptor));
                                 Put
                                   (Grammar, Descriptor, Goto_Set, Show_Lookaheads => True, Show_Goto_List => True);
                              end if;

                              --  else already in goto set
                           end if;
                        end if;
                     end;
                  end loop;
               end loop;
            end if;
         end if; -- item.dot /= null

         Item := Next (Item);
      end loop;

      return Goto_Set;
   end LALR_Goto_Transitions;

   function LALR_Kernels
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      First      : in Token_Array_Token_Set;
      Descriptor : in LALR_Descriptor)
     return LR1_Items.Item_Set_List
   is
      use LR1_Items;

      First_State_Index  : constant State_Index := 0;
      Kernels            : LR1_Items.Item_Set_List;
      New_Items_To_Check : Boolean              := True;
      New_Item_Set       : Item_Set;
      Found_State        : Unknown_State_Index;
   begin
      Kernels.Set_First (First_State_Index);
      Kernels.Set_Last (First_State_Index);
      Kernels (First_State_Index) :=
        (Set           => New_Item_Node
           (Prod       => (Grammar.First_Index, 0),
            Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First,
            State      => First_State_Index,
            Lookaheads => Null_Lookahead (Descriptor)),
         Goto_List     => <>,
         State         => First_State_Index);

      while New_Items_To_Check loop

         New_Items_To_Check   := False;

         for Checking_Set in reverse Kernels.First_Index .. Kernels.Last_Index loop
            --  Note that Kernels.Last_Index is not updated to reflect kernels
            --  added in the body of the loop. Reverse for consistency with a
            --  previous version that used a linked list for Kernels.

            if Trace_Generate > Detail then
               Ada.Text_IO.Put ("Checking ");
               Put (Grammar, Descriptor, Kernels (Checking_Set));
            end if;

            for Symbol in Descriptor.First_Terminal .. Descriptor.Last_Nonterminal loop

               New_Item_Set := LALR_Goto_Transitions (Kernels (Checking_Set), Symbol, First, Grammar, Descriptor);

               if New_Item_Set.Set /= null then

                  Found_State := Find (New_Item_Set, Kernels, Match_Lookaheads => False);

                  if Found_State = Unknown_State then
                     New_Items_To_Check := True;

                     New_Item_Set.State := Kernels.Last_Index + 1;

                     Set_State (New_Item_Set.Set, New_Item_Set.State);

                     Kernels.Append (New_Item_Set);

                     if Trace_Generate > Detail then
                        Ada.Text_IO.Put_Line ("  adding state" & Unknown_State_Index'Image (New_Item_Set.State));
                     end if;

                     Add (Kernels (Checking_Set).Goto_List, Symbol, New_Item_Set.State);
                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In ((Symbol, Found_State), Kernels (Checking_Set).Goto_List) then
                        if Trace_Generate > Detail then
                           Ada.Text_IO.Put_Line
                             ("  state" & Unknown_State_Index'Image (Kernels (Checking_Set).State) &
                                " adding goto on " & Image (Symbol, Descriptor) & " to state" &
                                Unknown_State_Index'Image (Found_State));

                        end if;

                        Add (Kernels (Checking_Set).Goto_List, Symbol, Found_State);
                     end if;
                  end if;
               end if;
            end loop;
         end loop;
      end loop;

      if Trace_Generate > Detail then
         Ada.Text_IO.New_Line;
      end if;

      return Kernels;
   end LALR_Kernels;

   --  Add propagation entries (if they don't already exist) from From
   --  to all kernel items that match To_Prod, To_Dot.
   procedure Add_Propagations
     (From         : in     LR1_Items.Item_Ptr;
      From_Set     : in     LR1_Items.Item_Set;
      To_Prod      : in     Production_ID;
      To_Dot       : in     Token_ID_Arrays.Cursor;
      For_Token    : in     Token_ID;
      Propagations : in out Item_Item_List_Mapping_Ptr;
      Kernels      : in out LR1_Items.Item_Set_List)
   is
      use all type Token_ID_Arrays.Cursor;
      use all type LR1_Items.Item_Ptr;

      Goto_State : constant Unknown_State_Index := LR1_Items.Goto_State (From_Set, For_Token);
      To_Kernel  : constant LR1_Items.Item_Ptr  :=
        (if Goto_State = Unknown_State then null
         else LR1_Items.Find (To_Prod, To_Dot, Kernels (Goto_State), Match_Lookaheads => False));

      Prop_Match    : Item_Item_List_Mapping_Ptr := Propagations;
      Prop_To_Match : Item_List_Ptr;
      Found_From    : Boolean                    := False;
      Found_To      : Boolean                    := False;
   begin
      if To_Kernel = null then
         return;
      end if;

      Find_Matching_Prop :
      while Prop_Match /= null loop
         if Prop_Match.From = From then

            Found_From    := True;
            Prop_To_Match := Prop_Match.To;
            while Prop_To_Match /= null loop

               --  ignore lookaheads in this match
               if Prod_ID (Prop_To_Match.Item) = Prod_ID (To_Kernel) and
                 Dot (Prop_To_Match.Item) = Dot (To_Kernel)
               then
                  Found_To := True;
                  exit Find_Matching_Prop;
               end if;
               Prop_To_Match := Prop_To_Match.Next;
            end loop;
            exit Find_Matching_Prop;
         end if;

         Prop_Match := Prop_Match.Next;
      end loop Find_Matching_Prop;

      if not Found_From then
         --  propagation for a new from_kernel
         Propagations := new Item_Item_List_Mapping'
           (From, new Item_List'(To_Kernel, Next => null), Next => Propagations);

      elsif not Found_To then
         --  add to propagations for an existing from_kernel
         Prop_Match.To := new Item_List'(To_Kernel, Next => Prop_Match.To);

      else
         raise Programmer_Error with "Add_Propagations: unexpected case";
      end if;
   end Add_Propagations;

   --  Calculate the lookaheads from Closure_Item for Source_Item.
   --  Source_Item must be one of the kernel items in Source_Set.
   --  Closure_Item must be an item in the lookahead closure of Source_Item for #.
   --
   --  Spontaneous lookaheads are put in Source_Item.Lookahead,
   --  propagated lookaheads in Propagations.
   --
   --  Set Used_Tokens = True for all tokens in lookaheads.
   procedure Generate_Lookahead_Info
     (Source_Item  :         in     LR1_Items.Item_Ptr;
      Source_Set   :         in     LR1_Items.Item_Set;
      Closure_Item :         in     LR1_Items.Item_Ptr;
      Propagations :         in out Item_Item_List_Mapping_Ptr;
      Used_Tokens  :         in out Token_ID_Set;
      Trace        :         in     Boolean;
      Descriptor   : aliased in     LALR_Descriptor;
      Grammar      :         in     WisiToken.Productions.Prod_Arrays.Vector;
      Kernels      :         in out LR1_Items.Item_Set_List)
   is
      use Token_ID_Arrays;
      use all type LR1_Items.Item_Ptr;

      Spontaneous_Count : Integer := 0;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("  closure_item: ");
         LR1_Items.Put (Grammar, Descriptor, Closure_Item);
         Ada.Text_IO.New_Line;
      end if;

      if Dot (Closure_Item) = No_Element then
         return;
      end if;

      declare
         ID          : constant Token_ID            := Element (Dot (Closure_Item));
         Next_Dot    : constant Cursor              := Next (Dot (Closure_Item));
         Goto_State  : constant Unknown_State_Index := LR1_Items.Goto_State (Source_Set, ID);
         Next_Kernel : constant LR1_Items.Item_Ptr  :=
           (if Goto_State = Unknown_State then null
            else LR1_Items.Find (Prod_ID (Closure_Item), Next_Dot, Kernels (Goto_State), Match_Lookaheads => False));
      begin
         begin
            Used_Tokens (ID) := True;
         exception
         when Constraint_Error =>
            raise Grammar_Error with "non-reporting " & Image (ID, Descriptor) & " used in grammar";
         end;

         if Lookaheads (Closure_Item) (Descriptor.Propagate_ID) then
            Add_Propagations
              (From         => Source_Item,
               From_Set     => Source_Set,
               To_Prod      => Prod_ID (Closure_Item),
               To_Dot       => Next_Dot,
               For_Token    => ID,
               Propagations => Propagations,
               Kernels      => Kernels);
         end if;

         if Next_Kernel /= null then
            if Trace then
               Spontaneous_Count := Spontaneous_Count + 1;
               Ada.Text_IO.Put_Line ("  spontaneous: " & Lookahead_Image (Lookaheads (Closure_Item), Descriptor));
            end if;

            LR1_Items.Include (Next_Kernel, Lookaheads (Closure_Item), Descriptor'Access, Exclude_Propagate => True);
         end if;

         if Spontaneous_Count > 0 then
            Ada.Text_IO.Put ("  Next_Kernel (" & Image (ID, Descriptor) & "): ");
            LR1_Items.Put (Grammar, Descriptor, Next_Kernel, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
         end if;
      end;
   end Generate_Lookahead_Info;

   procedure Propagate_Lookaheads
     (List       :         in Item_Item_List_Mapping_Ptr;
      Trace      :         in Boolean;
      Descriptor : aliased in WisiToken.Descriptor'Class;
      Grammar    :         in WisiToken.Productions.Prod_Arrays.Vector)
   is
      use all type LR1_Items.Item_Ptr;

      More_To_Check : Boolean := True;
      Mapping       : Item_Item_List_Mapping_Ptr;
      To            : Item_List_Ptr;
      Added_One     : Boolean;
      Added_Some    : Boolean := False;
   begin
      while More_To_Check loop

         More_To_Check := False;
         Mapping := List;
         while Mapping /= null loop

            To := Mapping.To;
            while To /= null loop
               LR1_Items.Include
                 (To.Item, Lookaheads (Mapping.From), Added_One, Descriptor'Access, Exclude_Propagate => True);

               if Trace and Added_One then
                  Added_Some := True;
                  Ada.Text_IO.Put ("  to: ");
                  LR1_Items.Put (Grammar, Descriptor, To.Item, Show_Lookaheads => True);
                  Ada.Text_IO.New_Line;
               end if;

               More_To_Check := More_To_Check or Added_One;
               To := To.Next;
            end loop;

            if Trace and Added_Some then
               Added_Some := False;
               Ada.Text_IO.Put ("from: ");
               LR1_Items.Put (Grammar, Descriptor, Mapping.From, Show_Lookaheads => True);
               Ada.Text_IO.New_Line;
            end if;

            Mapping := Mapping.Next;
         end loop;
      end loop;
   end Propagate_Lookaheads;

   --  Calculate the LALR(1) lookaheads for Grammar.
   --  Kernels should be the sets of LR(0) kernels on input, and will
   --  become the set of LALR(1) kernels on output.
   procedure Fill_In_Lookaheads
     (Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Kernels              : in out LR1_Items.Item_Set_List;
      Used_Tokens          : in out Token_ID_Set;
      Trace                : in     Boolean;
      Descriptor           : in     LALR_Descriptor)
   is
      use all type LR1_Items.Item_Ptr;

      Kernel_Item  : LR1_Items.Item_Ptr;
      Closure_Item : LR1_Items.Item_Ptr;

      Kernel_Item_Set : LR1_Items.Item_Set :=
        (Set       => new LR1_Items.Item_Node,
         Goto_List => <>,
         State     => Unknown_State);

      Closure : LR1_Items.Item_Set;

      Propagation_List : Item_Item_List_Mapping_Ptr;

   begin
      for Kernel of Kernels loop
         if Trace then
            Ada.Text_IO.Put ("Adding lookaheads for ");
            LR1_Items.Put (Grammar, Descriptor, Kernel);
         end if;

         Kernel_Item := Kernel.Set;

         while Kernel_Item /= null loop
            LR1_Items.Set
              (Kernel_Item_Set.Set.all,
               Prod_ID (Kernel_Item),
               Dot (Kernel_Item),
               State (Kernel_Item),
               Propagate_Lookahead (Descriptor));

            Closure := LR1_Items.Closure
              (Kernel_Item_Set, Has_Empty_Production, First, Grammar, Descriptor);

            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Kernel_Item, Kernel, Closure_Item, Propagation_List, Used_Tokens, Trace, Descriptor, Grammar,
                  Kernels);

               Closure_Item := Next (Closure_Item);
            end loop;

            LR1_Items.Free (Closure);
            Kernel_Item := Next (Kernel_Item);
         end loop;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Propagations:");
         Put (Grammar, Descriptor, Propagation_List);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagation_List, Trace, Descriptor, Grammar);

      Free (Propagation_List);
      LR1_Items.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

   --  Add actions for all Kernels to Table.
   procedure Add_Actions
     (Kernels              : in     LR1_Items.Item_Set_List;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Descriptor           : in     LALR_Descriptor)
   is
      Closure : LR1_Items.Item_Set;
   begin
      for Kernel of Kernels loop
         Closure := LR1_Items.Closure
           (Kernel, Has_Empty_Production, First, Grammar, Descriptor);

         Add_Actions
           (Closure, Table, Grammar, Has_Empty_Production, First, Conflicts,
            Trace_Generate > Detail, Descriptor);

         LR1_Items.Free (Closure);
      end loop;

      if Trace_Generate > Detail then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   function Generate
     (Grammar         : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor      : in LALR_Descriptor;
      Known_Conflicts : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      McKenzie_Param  : in McKenzie_Param_Type := Default_McKenzie_Param;
      Put_Parse_Table : in Boolean := False)
     return Parse_Table_Ptr
   is
      use all type Ada.Containers.Count_Type;

      Ignore_Unused_Tokens     : constant Boolean := WisiToken.Trace_Generate > 1;
      Ignore_Unknown_Conflicts : constant Boolean := WisiToken.Trace_Generate > 1;

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Grammar);

      First : constant Token_Array_Token_Set := WisiToken.Generate.First
        (Grammar, Has_Empty_Production, Descriptor.First_Terminal);

      Used_Tokens : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);

      Kernels : LR1_Items.Item_Set_List := LALR_Kernels (Grammar, First, Descriptor);

      Unused_Tokens        : Boolean             := False;
      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;

   begin
      WisiToken.Generate.Error := False; -- necessary in unit tests; some previous test might have encountered an error.

      Used_Tokens (Grammar (Grammar.First_Index).LHS) := True;

      Fill_In_Lookaheads
        (Grammar, Has_Empty_Production, First, Kernels, Used_Tokens, Trace_Generate > Detail,
         Descriptor);

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "Unused tokens:");
               Unused_Tokens := True;
            end if;
            Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, Image (I, Descriptor));
         end if;
      end loop;

      if Unused_Tokens then
         WisiToken.Generate.Error := not Ignore_Unused_Tokens;
         Ada.Text_IO.New_Line;
      end if;

      if Trace_Generate > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Kernels:");
         LR1_Items.Put (Grammar, Descriptor, Kernels, Show_Lookaheads => True);
      end if;

      Table := new Parse_Table
        (State_First       => Kernels.First_Index,
         State_Last        => Kernels.Last_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      if McKenzie_Param = Default_McKenzie_Param then
         --  Descriminants in Default are wrong
         Table.McKenzie_Param :=
           (First_Terminal    => Descriptor.First_Terminal,
            Last_Terminal     => Descriptor.Last_Terminal,
            First_Nonterminal => Descriptor.First_Nonterminal,
            Last_Nonterminal  => Descriptor.Last_Nonterminal,
            Insert            => (others => 0),
            Delete            => (others => 0),
            Push_Back         => (others => 0),
            Task_Count        => Default_McKenzie_Param.Task_Count,
            Cost_Limit        => Default_McKenzie_Param.Cost_Limit,
            Check_Limit       => Default_McKenzie_Param.Check_Limit,
            Check_Delta_Limit => Default_McKenzie_Param.Check_Delta_Limit,
            Enqueue_Limit     => Default_McKenzie_Param.Enqueue_Limit);
      else
         Table.McKenzie_Param := McKenzie_Param;
      end if;

      Generate_Utils.Compute_Minimal_Terminal_Sequences (Grammar, Descriptor, Table.Minimal_Terminal_Sequences);

      Add_Actions (Kernels, Grammar, Has_Empty_Production, First, Unknown_Conflicts, Table.all, Descriptor);

      --  Set Table.States.Productions for McKenzie_Recover
      for State in Table.States'Range loop
         Table.States (State).Productions := LR1_Items.Productions (Kernels (State));
      end loop;

      if Put_Parse_Table then
         LALR_Generate.Put_Parse_Table (Table, Grammar, Kernels, Descriptor);
      end if;

      Delete_Known (Unknown_Conflicts, Known_Conflicts_Edit);

      if Unknown_Conflicts.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "unknown conflicts:");
         Put (Unknown_Conflicts, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Unknown_Conflicts;
      end if;

      if Known_Conflicts_Edit.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "excess known conflicts:");
         Put (Known_Conflicts_Edit, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         WisiToken.Generate.Error := WisiToken.Generate.Error or not Ignore_Unknown_Conflicts;
      end if;

      return Table;
   end Generate;

end WisiToken.LR.LALR_Generate;
