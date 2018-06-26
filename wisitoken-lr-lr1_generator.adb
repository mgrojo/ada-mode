--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Stephe Leake
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
with WisiToken.Generate;
package body WisiToken.LR.LR1_Generator is

   function LR1_Goto_Transitions
     (Set                  : in LR1_Items.Item_Set;
      Symbol               : in Token_ID;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor           : in WisiToken.Descriptor;
      Trace                : in Boolean)
     return LR1_Items.Item_Set
   is
      use Token_ID_Arrays;
      use LR1_Items;

      Goto_Set : Item_Set;
      Item     : Item_Ptr := Set.Set;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop
         if Dot (Item) /= No_Element then
            if Element (Dot (Item)) = Symbol and
              --  We don't need a state with dot after EOF in the
              --  accept production. EOF should only appear in the
              --  accept production.
              Symbol /= Descriptor.EOF_ID
            then
               Add
                 (Goto_Set.Set,
                  New_Item_Node (Prod_ID (Item), Next (Dot (Item)), Unknown_State, Lookaheads (Item)));
            end if;
         end if;

         Item := Next (Item);
      end loop;

      if Goto_Set.Set /= null then
         if Trace then
            Ada.Text_IO.Put_Line ("LR1_Goto_Transitions " & Image (Symbol, Descriptor));
            Put (Grammar, Descriptor, Goto_Set, Show_Lookaheads => True);
         end if;

         return Closure (Goto_Set, Has_Empty_Production, First, Grammar, Descriptor, Trace => False);
      else
         return Goto_Set;
      end if;
   end LR1_Goto_Transitions;

   function LR1_Item_Sets
     (Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in WisiToken.Productions.Prod_Arrays.Vector;
      First_State_Index    : in State_Index;
      Descriptor           : in WisiToken.Descriptor;
      Trace                : in Boolean)
     return LR1_Items.Item_Set_List
   is
      use LR1_Items;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "items"

      C : Item_Set_List := -- result
        (Head                 => new Item_Set'
           (Closure
              ((Set           => New_Item_Node
                  (Prod       => (Grammar.First_Index, 0),
                   Dot        => Grammar (Grammar.First_Index).RHSs (0).Tokens.First,
                   State      => First_State_Index,
                   Lookaheads => To_Lookahead (Descriptor.EOF_ID, Descriptor)),
                Goto_List     => null,
                State         => First_State_Index,
                Next          => null),
               Has_Empty_Production, First, Grammar, Descriptor,
               Trace          => False)),
         Size                 => 1);

      I          : Item_Set_Ptr;    -- iterator 'for each set of items I in C'
      Added_Item : Boolean := True; -- 'until no more items can be added'

      New_Items     : Item_Set;
      New_Items_Set : Item_Set_Ptr;

   begin
      loop
         Added_Item   := False;
         I := C.Head;

         while I /= null loop
            if Trace then
               Ada.Text_IO.Put ("Checking ");
               Put (Grammar, Descriptor, I.all, Show_Lookaheads => True, Show_Goto_List => True);
            end if;

            for Symbol in Descriptor.First_Terminal .. Descriptor.Last_Nonterminal loop -- 'for each grammar symbol X'

               New_Items := LR1_Goto_Transitions
                 (I.all, Symbol, Has_Empty_Production, First, Grammar, Descriptor, Trace);

               if New_Items.Set /= null then -- 'goto (I, X) not empty'

                  New_Items_Set := Find (New_Items, C, Match_Lookaheads => True); -- 'not in C'

                  if New_Items_Set = null then
                     Added_Item := True;

                     New_Items.Next  := C.Head;
                     New_Items.State := C.Size + First_State_Index;

                     Set_State (New_Items.Set, New_Items.State);

                     if Trace then
                        Ada.Text_IO.Put_Line ("  adding state" & Unknown_State_Index'Image (New_Items.State));
                     end if;

                     C :=
                       (Head => new Item_Set'(New_Items),
                        Size => C.Size + 1);

                     Add (I.Goto_List, Symbol, C.Head);
                  else

                     --  If there's not already a goto entry between these two sets, create one.
                     if not Is_In
                       (Symbol    => Symbol,
                        Set       => New_Items_Set,
                        Goto_List => I.Goto_List)
                     then
                        if Trace then
                           Ada.Text_IO.Put_Line
                             ("  adding goto on " & Image (Symbol, Descriptor) & " to state" &
                                Unknown_State_Index'Image (New_Items_Set.State));

                        end if;

                        Add (I.Goto_List, Symbol, New_Items_Set);
                     end if;

                     --  The set is already there, so we don't need this copy.
                     Free (New_Items);
                  end if;
               end if;
            end loop;

            I := I.Next;
         end loop;
         exit when not Added_Item;

      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;

      return Reverse_List (C);
   end LR1_Item_Sets;

   procedure Add_Actions
     (Item_Sets            : in     LR1_Items.Item_Set_List;
      Grammar              : in     WisiToken.Productions.Prod_Arrays.Vector;
      Has_Empty_Production : in     Token_ID_Set;
      First                : in     Token_Array_Token_Set;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean;
      Descriptor           : in     WisiToken.Descriptor)
   is
      --  Add actions for all Item_Sets to Table.

      Item_Set : LR1_Items.Item_Set_Ptr := Item_Sets.Head;
      use type LR1_Items.Item_Set_Ptr;
   begin
      while Item_Set /= null loop
         Add_Actions (Item_Set.all, Table, Grammar, Has_Empty_Production, First, Conflicts, Trace, Descriptor);
         Item_Set := Item_Set.Next;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   procedure Put_Parse_Table
     (Table      : in Parse_Table_Ptr;
      Item_Sets  : in LR1_Items.Item_Set_List;
      Descriptor : in WisiToken.Descriptor;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("LR1 Parse Table:");

      if Table.McKenzie_Param.Cost_Limit /= Default_McKenzie_Param.Cost_Limit then
         Put_Line ("McKenzie:");
         Put (Table.McKenzie_Param, Descriptor);
         New_Line;
      end if;

      Put_Line ("Minimal_Terminal_Sequences:");
      for I in Table.Minimal_Terminal_Sequences.First_Index .. Table.Minimal_Terminal_Sequences.Last_Index loop
         Put_Line (Image (I, Descriptor) & " => " & Image (Table.Minimal_Terminal_Sequences (I), Descriptor));
      end loop;
      New_Line;

      for State in Table.States'Range loop
         LR1_Items.Put
           (Grammar, Descriptor, LR1_Items.Find (State, Item_Sets).all, Kernel_Only => True, Show_Lookaheads => True);
         New_Line;
         Put (Descriptor, Table.States (State));

         New_Line;
      end loop;
   end Put_Parse_Table;

   function Check_Unused_Tokens
     (Descriptor : in WisiToken.Descriptor;
      Grammar    : in WisiToken.Productions.Prod_Arrays.Vector)
     return Boolean
   is
      Used_Tokens : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);

      Unused_Tokens : Boolean := False;
   begin
      Used_Tokens (Descriptor.Accept_ID) := True;

      for Prod of Grammar loop
         for RHS of Prod.RHSs loop
            for J of RHS.Tokens loop
               Used_Tokens (J) := True;
            end loop;
         end loop;
      end loop;

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "Unused tokens:");
               Unused_Tokens := True;
            end if;
            Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, Image (I, Descriptor));
         end if;
      end loop;

      return Unused_Tokens;
   end Check_Unused_Tokens;

   function Generate
     (Grammar                  : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor               : in WisiToken.Descriptor;
      First_State_Index        : in State_Index;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      McKenzie_Param           : in McKenzie_Param_Type := Default_McKenzie_Param;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr
   is
      use type Ada.Containers.Count_Type;

      Unused_Tokens : constant Boolean := Check_Unused_Tokens (Descriptor, Grammar);

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant Token_ID_Set := WisiToken.Generate.Has_Empty_Production (Grammar, Descriptor);

      First                : constant Token_Array_Token_Set := LR1_Items.First
        (Grammar, Descriptor, Has_Empty_Production, Trace_Generate > Detail);

      Item_Sets : constant LR1_Items.Item_Set_List := LR1_Item_Sets
        (Has_Empty_Production, First, Grammar, First_State_Index, Descriptor, Trace_Generate > Detail);

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;
   begin
      if Trace_Generate > Detail then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Item_Sets:");
         LR1_Items.Put (Grammar, Descriptor, Item_Sets);
      end if;

      Table := new Parse_Table
        (State_First       => First_State_Index,
         State_Last        => Item_Sets.Size - 1 + First_State_Index,
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

      Generator_Utils.Compute_Minimal_Terminal_Sequences (Grammar, Descriptor, Table.Minimal_Terminal_Sequences);

      Add_Actions
        (Item_Sets, Grammar, Has_Empty_Production, First, Unknown_Conflicts, Table.all,
         Trace_Generate > Detail, Descriptor);

      if Trace_Generate > Outline then
         LR1_Generator.Put_Parse_Table (Table, Item_Sets, Descriptor, Grammar);
      end if;

      Delete_Known (Unknown_Conflicts, Known_Conflicts_Edit);

      if Unknown_Conflicts.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "unknown conflicts:");
         Put (Unknown_Conflicts, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         Generator_Utils.Error := Generator_Utils.Error or not Ignore_Unknown_Conflicts;
      end if;

      if Known_Conflicts_Edit.Length > 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error, "excess known conflicts:");
         Put (Known_Conflicts_Edit, Ada.Text_IO.Current_Error, Descriptor);
         Ada.Text_IO.New_Line (Ada.Text_IO.Current_Error);
         Generator_Utils.Error := Generator_Utils.Error or not Ignore_Unknown_Conflicts;
      end if;

      Generator_Utils.Error := Generator_Utils.Error or (Unused_Tokens and not Ignore_Unused_Tokens);

      if Generator_Utils.Error then
         raise Grammar_Error with "errors: aborting";
      end if;

      return Table;
   end Generate;

end WisiToken.LR.LR1_Generator;
