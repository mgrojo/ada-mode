--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2002 - 2005, 2008 - 2015, 2017 Stephe Leake
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
package body WisiToken.Parser.LR.LALR_Generator is

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
     (Descriptor   : in WisiToken.Descriptor'Class;
      Propagations : in Item_Item_List_Mapping_Ptr)
   is
      Next_Prop : Item_Item_List_Mapping_Ptr := Propagations;
      Next_To   : Item_List_Ptr;
   begin
      while Next_Prop /= null loop

         Ada.Text_IO.Put ("From ");
         LR1_Items.Put (Descriptor, Next_Prop.From, Show_Lookaheads => True);
         Ada.Text_IO.New_Line;

         Next_To := Next_Prop.To;
         while Next_To /= null loop
            Ada.Text_IO.Put ("To   ");
            LR1_Items.Put (Descriptor, Next_To.Item, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;

            Next_To := Next_To.Next;
         end loop;

         Next_Prop := Next_Prop.Next;
      end loop;

   end Put;

   procedure Put_Parse_Table
     (Table      : in Parse_Table_Ptr;
      Kernels    : in LR1_Items.Item_Set_List;
      Descriptor : in LALR_Descriptor)
   is
      use all type WisiToken.Parser.LR.LR1_Items.Item_Set_Ptr;
      use Ada.Text_IO;
      Kernel : WisiToken.Parser.LR.LR1_Items.Item_Set_Ptr;
   begin
      Put_Line ("LALR Parse Table:");
      Put_Line ("Panic_Recover:");
      Put (Descriptor, Table.Panic_Recover);
      New_Line;

      Put_Line ("Follow:");
      Put (Descriptor, Table.Follow);
      New_Line;

      for State in Table.States'Range loop
         Kernel := LR1_Items.Find (State, Kernels);
         if Kernel = null then
            raise Programmer_Error with "state" & Unknown_State_Index'Image (State) & " not found in kernels";
         else
            LR1_Items.Put (Descriptor, Kernel.all, Show_Lookaheads => True);
         end if;
         New_Line;
         Put (Descriptor, Table.States (State));

         New_Line;
      end loop;
   end Put_Parse_Table;

   ----------
   --  Generator utils

   function LALR_Goto_Transitions
     (Kernel     : in LR1_Items.Item_Set;
      Symbol     : in Token_ID;
      First      : in Token_Array_Token_Set;
      Grammar    : in Production.List.Instance;
      Descriptor : in LALR_Descriptor;
      Trace      : in Boolean)
     return LR1_Items.Item_Set
   is
      use Token.List;
      use LR1_Items;

      Goto_Set : Item_Set;

      Item   : Item_Ptr := Kernel.Set;
      Dot_ID : Token_ID;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop

         if Dot (Item) /= Null_Iterator then

            Dot_ID := ID (Dot (Item));
            --  ID of token after Dot

            --  If Symbol = EOF_Token, this is the start symbol accept
            --  production; don't need a kernel with dot after EOF.
            if Dot_ID = Symbol and
              Symbol /= Descriptor.EOF_ID and
              null = Find (Prod (Item), Next (Dot (Item)), Goto_Set, Match_Lookaheads => False)
            then
               Add
                 (Goto_Set.Set,
                  New_Item_Node
                    (Prod       => Prod (Item),
                     Dot        => Next (Dot (Item)),
                     State      => Unknown_State, -- replaced in Kernels
                     Lookaheads => Lookaheads (Item)));

               if Trace then
                  Ada.Text_IO.Put_Line ("LALR_Goto_Transitions " & Image (Descriptor, Symbol));
                  Put (Descriptor, Goto_Set, Show_Lookaheads => True, Show_Goto_List => True);
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
               declare
                  Prod_I : Production.List.List_Iterator := Production.List.First (Grammar);
                  Prod   : Production.Instance;
                  RHS_I  : Token.List.List_Iterator;
               begin
                  while not Production.List.Is_Done (Prod_I) loop
                     Prod  := Production.List.Current (Prod_I);
                     RHS_I := Prod.RHS.Tokens.First;

                     if (Dot_ID = Prod.LHS or First (Dot_ID, Prod.LHS)) and
                       (RHS_I /= Null_Iterator and then ID (RHS_I) = Symbol)
                     then
                        if null = Find
                          (Prod, Next (RHS_I), Goto_Set, Match_Lookaheads => False)
                        then
                           Add
                             (Goto_Set.Set,
                              New_Item_Node
                                (Prod       => Prod,
                                 Dot        => Next (RHS_I),
                                 State      => Unknown_State, -- replaced in Kernels
                                 Lookaheads => Null_Lookahead (Descriptor)));

                           --  else already in goto set

                           if Trace then
                              Ada.Text_IO.Put_Line ("LALR_Goto_Transitions " & Image (Descriptor, Symbol));
                              Put (Descriptor, Goto_Set, Show_Lookaheads => True, Show_Goto_List => True);
                           end if;
                        end if;
                     end if;

                     Production.List.Next (Prod_I);
                  end loop;
               end;
            end if;
         end if; -- item.dot /= null

         Item := Next (Item);
      end loop;

      return Goto_Set;
   end LALR_Goto_Transitions;

   function LALR_Kernels
     (Grammar           : in Production.List.Instance;
      First             : in Token_Array_Token_Set;
      First_State_Index : in State_Index;
      Descriptor        : in LALR_Descriptor;
      Trace             : in Boolean)
     return LR1_Items.Item_Set_List
   is
      use LR1_Items;
      use type Token.List.List_Iterator;

      Kernel_List : Item_Set_List :=
        (Head         => new Item_Set'
           (Set       => New_Item_Node
              (Production.List.Current (Production.List.First (Grammar)),
               Production.List.RHS (Production.List.First (Grammar)).Tokens.First,
               First_State_Index,
               Null_Lookahead (Descriptor)),
            Goto_List => null,
            State     => First_State_Index,
            Next      => null),
         Size         => 1);

      New_Items_To_Check : Boolean := True;
      Checking_Set       : Item_Set_Ptr;
      New_Items          : Item_Set;
      New_Items_Set      : Item_Set_Ptr;

   begin

      while New_Items_To_Check loop

         New_Items_To_Check   := False;

         --  For all items in the kernel list that haven't been checked yet...
         Checking_Set := Kernel_List.Head;
         while Checking_Set /= null loop
            if Trace then
               Ada.Text_IO.Put ("Checking ");
               Put (Descriptor, Checking_Set.all);
            end if;

            for Symbol in Descriptor.First_Terminal .. Descriptor.Last_Nonterminal loop

               New_Items := LALR_Goto_Transitions (Checking_Set.all, Symbol, First, Grammar, Descriptor, Trace);

               if New_Items.Set /= null then

                  New_Items_Set := Find (New_Items, Kernel_List, Match_Lookaheads => False);

                  if New_Items_Set = null then
                     New_Items_To_Check := True;

                     New_Items.Next  := Kernel_List.Head;
                     New_Items.State := Kernel_List.Size + First_State_Index;

                     Set_State (New_Items.Set, New_Items.State);

                     if Trace then
                        Ada.Text_IO.Put_Line ("  adding state" & Unknown_State_Index'Image (New_Items.State));
                     end if;

                     Kernel_List :=
                       (Head => new Item_Set'(New_Items),
                        Size => Kernel_List.Size + 1);

                     Add (Checking_Set.Goto_List, Symbol, Kernel_List.Head);
                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Symbol    => Symbol,
                        Set       => New_Items_Set,
                        Goto_List => Checking_Set.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put_Line
                             ("  state" & Unknown_State_Index'Image (Checking_Set.State) &
                                " adding goto on " & Image (Descriptor, Symbol) & " to state" &
                                Unknown_State_Index'Image (New_Items_Set.State));

                        end if;

                        Add (Checking_Set.Goto_List, Symbol, New_Items_Set);
                     end if;

                     --  The set is already there, so we don't need this copy.
                     Free (New_Items);
                  end if;
               end if;
            end loop;

            Checking_Set := Checking_Set.Next;
         end loop;

      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Reverse_List (Kernel_List);
   end LALR_Kernels;

   --  Add propagation entries (if they don't already exist) from From
   --  to all kernel items that match To_Prod, To_Dot.
   procedure Add_Propagations
     (From         : in     LR1_Items.Item_Ptr;
      From_Set     : in     LR1_Items.Item_Set;
      To_Prod      : in     Production.Instance;
      To_Dot       : in     Token.List.List_Iterator;
      For_Token    : in     Token_ID;
      Propagations : in out Item_Item_List_Mapping_Ptr)
   is
      use all type Production.Instance;
      use all type Token.List.List_Iterator;
      use all type LR1_Items.Item_Set_Ptr;
      use all type LR1_Items.Item_Ptr;

      Goto_Set  : constant LR1_Items.Item_Set_Ptr := LR1_Items.Goto_Set (From_Set, For_Token);
      To_Kernel : constant LR1_Items.Item_Ptr     :=
        (if Goto_Set = null then null
         else LR1_Items.Find (To_Prod, To_Dot, Goto_Set.all, Match_Lookaheads => False));

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
               if Prod (Prop_To_Match.Item) = Prod (To_Kernel) and
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
      Descriptor   : aliased in     LALR_Descriptor)
   is
      use all type LR1_Items.Item_Ptr;
      use all type LR1_Items.Item_Set_Ptr;
      use all type Token.List.List_Iterator;

      Spontaneous_Count : Integer := 0;
   begin
      if Trace then
         Ada.Text_IO.Put_Line ("  closure_item: ");
         LR1_Items.Put (Descriptor, Closure_Item);
         Ada.Text_IO.New_Line;
      end if;

      if Dot (Closure_Item) = Token.List.Null_Iterator then
         return;
      end if;

      declare
         ID          : constant Token_ID                 := Token.List.ID (Dot (Closure_Item));
         Next_Dot    : constant Token.List.List_Iterator := Token.List.Next (Dot (Closure_Item));
         Goto_Set    : constant LR1_Items.Item_Set_Ptr   := LR1_Items.Goto_Set (Source_Set, ID);
         Next_Kernel : constant LR1_Items.Item_Ptr       :=
           (if Goto_Set = null then null
            else LR1_Items.Find (Prod (Closure_Item), Next_Dot, Goto_Set.all, Match_Lookaheads => False));
      begin
         begin
            Used_Tokens (ID) := True;
         exception
         when Constraint_Error =>
            raise Grammar_Error with "non-reporting " & Image (Descriptor, ID) & " used in grammar";
         end;

         if Lookaheads (Closure_Item) (Descriptor.Propagate_ID) then
            Add_Propagations
              (From         => Source_Item,
               From_Set     => Source_Set,
               To_Prod      => Prod (Closure_Item),
               To_Dot       => Next_Dot,
               For_Token    => ID,
               Propagations => Propagations);
         end if;

         if Next_Kernel /= null then
            if Trace then
               Spontaneous_Count := Spontaneous_Count + 1;
               Ada.Text_IO.Put_Line ("  spontaneous: " & Lookahead_Image (Descriptor, Lookaheads (Closure_Item)));
            end if;

            LR1_Items.Include (Next_Kernel, Lookaheads (Closure_Item), Descriptor'Access, Exclude_Propagate => True);
         end if;

         if Spontaneous_Count > 0 then
            Ada.Text_IO.Put ("  Next_Kernel (" & Image (Descriptor, ID) & "): ");
            LR1_Items.Put (Descriptor, Next_Kernel, Show_Lookaheads => True);
            Ada.Text_IO.New_Line;
         end if;
      end;
   end Generate_Lookahead_Info;

   procedure Propagate_Lookaheads
     (List       :         in Item_Item_List_Mapping_Ptr;
      Trace      :         in Boolean;
      Descriptor : aliased in WisiToken.Descriptor'Class)
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
                  LR1_Items.Put (Descriptor, To.Item, Show_Lookaheads => True);
                  Ada.Text_IO.New_Line;
               end if;

               More_To_Check := More_To_Check or Added_One;
               To := To.Next;
            end loop;

            if Trace and Added_Some then
               Added_Some := False;
               Ada.Text_IO.Put ("from: ");
               LR1_Items.Put (Descriptor, Mapping.From, Show_Lookaheads => True);
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
     (Grammar              : in     Production.List.Instance;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Kernels              : in out LR1_Items.Item_Set_List;
      Used_Tokens          : in out Token_ID_Set;
      Trace                : in     Boolean;
      Descriptor           : in     LALR_Descriptor)
   is
      use all type LR1_Items.Item_Set_Ptr;
      use all type LR1_Items.Item_Ptr;

      Kernel       : LR1_Items.Item_Set_Ptr := Kernels.Head;
      Kernel_Item  : LR1_Items.Item_Ptr;
      Closure_Item : LR1_Items.Item_Ptr;

      Kernel_Item_Set : LR1_Items.Item_Set :=
        (Set       => new LR1_Items.Item_Node,
         Goto_List => null,
         State     => Unknown_State,
         Next      => null);

      Closure : LR1_Items.Item_Set;

      Propagation_List : Item_Item_List_Mapping_Ptr;

   begin
      while Kernel /= null loop
         if Trace then
            Ada.Text_IO.Put ("Adding lookaheads for ");
            LR1_Items.Put (Descriptor, Kernel.all);
         end if;

         Kernel_Item := Kernel.Set;

         while Kernel_Item /= null loop
            LR1_Items.Set
              (Kernel_Item_Set.Set.all,
               Prod (Kernel_Item),
               Dot (Kernel_Item),
               State (Kernel_Item),
               Propagate_Lookahead (Descriptor));

            Closure := LR1_Items.Closure
              (Kernel_Item_Set, Has_Empty_Production, First, Grammar, Descriptor, Trace => False);

            Closure_Item := Closure.Set;
            while Closure_Item /= null loop

               Generate_Lookahead_Info
                 (Kernel_Item, Kernel.all, Closure_Item, Propagation_List, Used_Tokens, Trace, Descriptor);

               Closure_Item := Next (Closure_Item);
            end loop;

            LR1_Items.Free (Closure);
            Kernel_Item := Next (Kernel_Item);
         end loop;

         Kernel := Kernel.Next;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Propagations:");
         Put (Descriptor, Propagation_List);
         Ada.Text_IO.New_Line;
      end if;

      Propagate_Lookaheads (Propagation_List, Trace, Descriptor);

      Free (Propagation_List);
      LR1_Items.Free (Kernel_Item_Set);

   end Fill_In_Lookaheads;

   --  Add actions for all Kernels to Table.
   procedure Add_Actions
     (Kernels              : in     LR1_Items.Item_Set_List;
      Grammar              : in     Production.List.Instance;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean;
      Descriptor           : in     LALR_Descriptor)
   is
      use type LR1_Items.Item_Set_Ptr;

      Kernel  : LR1_Items.Item_Set_Ptr := Kernels.Head;
      Closure : LR1_Items.Item_Set;
   begin
      while Kernel /= null loop
         Closure := LR1_Items.Closure
           (Kernel.all, Has_Empty_Production, First, Grammar, Descriptor, Trace => False);

         Add_Actions (Closure, Table, Has_Empty_Production, Conflicts, Trace, Descriptor);
         Kernel := Kernel.Next;

         LR1_Items.Free (Closure);
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   function Generate
     (Grammar                  : in Production.List.Instance;
      Descriptor               : in LALR_Descriptor;
      First_State_Index        : in State_Index;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Panic_Recover            : in Token_ID_Set        := Default_Panic_Recover;
      McKenzie_Param           : in McKenzie_Param_Type := Default_McKenzie_Param;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr
   is
      use all type Ada.Containers.Count_Type;
      use all type Production.Instance;
      use all type LR1_Items.Item_Set_Ptr;
      use all type LR1_Items.Item_Ptr;

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant Token_ID_Set := LR1_Items.Has_Empty_Production (Grammar, Descriptor);

      First : constant Token_Array_Token_Set := LR1_Items.First (Grammar, Descriptor, Has_Empty_Production, Trace);

      Used_Tokens : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);

      Kernels : LR1_Items.Item_Set_List := LALR_Kernels (Grammar, First, First_State_Index, Descriptor, Trace);

      First_Production : Production.Instance renames Production.List.Current (Grammar.First);

      Unused_Tokens        : Boolean             := False;
      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;

   begin
      Used_Tokens (First_Production.LHS) := True;

      Fill_In_Lookaheads (Grammar, Has_Empty_Production, First, Kernels, Used_Tokens, Trace, Descriptor);

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               Ada.Text_IO.Put_Line ("Unused tokens:");
               Unused_Tokens := True;
            end if;
            Ada.Text_IO.Put_Line (Image (Descriptor, I));
         end if;
      end loop;
      if Unused_Tokens then
         Ada.Text_IO.New_Line;
      end if;

      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Kernels:");
         LR1_Items.Put (Descriptor, Kernels, Show_Lookaheads => True);
      end if;

      Table := new Parse_Table
        (State_First       => First_State_Index,
         State_Last        => Kernels.Size - 1 + First_State_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      Table.Panic_Recover :=
        (if Panic_Recover = Default_Panic_Recover
         then (Table.First_Nonterminal .. Table.Last_Nonterminal => False)
         else Panic_Recover);

      Table.McKenzie := McKenzie_Param;

      Table.Follow := LR1_Items.Follow (Grammar, Descriptor, First, Has_Empty_Production);

      Add_Actions
        (Kernels, Grammar, Has_Empty_Production, First, Unknown_Conflicts, Table.all, Trace, Descriptor);

      if Put_Parse_Table then
         LALR_Generator.Put_Parse_Table (Table, Kernels, Descriptor);
      end if;

      Delete_Known (Unknown_Conflicts, Known_Conflicts_Edit);

      if Unknown_Conflicts.Length > 0 then
         Ada.Text_IO.Put_Line ("unknown conflicts:");
         Put (Descriptor, Unknown_Conflicts);
         if not Ignore_Unknown_Conflicts then
            raise Grammar_Error with "unknown conflicts; aborting";
         end if;
      end if;

      if Known_Conflicts_Edit.Length > 0 then
         Ada.Text_IO.Put_Line ("excess known conflicts:");
         Put (Descriptor, Known_Conflicts_Edit);
         if not Ignore_Unknown_Conflicts then
            raise Grammar_Error with "excess known conflicts; aborting";
         end if;
      end if;

      if Unused_Tokens and not (Trace or Ignore_Unused_Tokens) then
         raise Grammar_Error with "unused tokens; aborting";
      end if;

      return Table;
   end Generate;

end WisiToken.Parser.LR.LALR_Generator;
