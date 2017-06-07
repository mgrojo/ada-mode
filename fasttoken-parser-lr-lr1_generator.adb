--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephe Leake
--
--  This file is part of the FastToken package.
--
--  The FastToken package is free software; you can redistribute it
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
package body FastToken.Parser.LR.LR1_Generator is

   function LR1_Goto_Transitions
     (Set                  : in LR1_Items.Item_Set;
      Symbol               : in Token_ID;
      Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in Production.List.Instance;
      Descriptor           : in FastToken.Descriptor;
      Trace                : in Boolean)
     return LR1_Items.Item_Set
   is
      use Token.List;
      use LR1_Items;

      Goto_Set : Item_Set;
      Item     : Item_Ptr := Set.Set;
   begin
      Goto_Set.State := Unknown_State;

      while Item /= null loop
         if Dot (Item) /= Null_Iterator then
            if ID (Dot (Item)) = Symbol and
              --  We don't need a state with dot after EOF in the
              --  accept production. EOF should only appear in the
              --  accept production.
              Symbol /= Descriptor.EOF_ID
            then
               Add
                 (Goto_Set.Set,
                  New_Item_Node (Prod (Item), Next (Dot (Item)), Unknown_State, Lookaheads (Item)));
            end if;
         end if;

         Item := Next (Item);
      end loop;

      if Goto_Set.Set /= null then
         if Trace then
            Ada.Text_IO.Put_Line ("LR1_Goto_Transitions " & Image (Descriptor, Symbol));
            Put (Descriptor, Goto_Set, Show_Lookaheads => True);
         end if;

         return Closure (Goto_Set, Has_Empty_Production, First, Grammar, Descriptor, Trace => False);
      else
         return Goto_Set;
      end if;
   end LR1_Goto_Transitions;

   function LR1_Item_Sets
     (Has_Empty_Production : in Token_ID_Set;
      First                : in Token_Array_Token_Set;
      Grammar              : in Production.List.Instance;
      First_State_Index    : in State_Index;
      Descriptor           : in FastToken.Descriptor;
      Trace                : in Boolean)
     return LR1_Items.Item_Set_List
   is
      use LR1_Items;
      use type Token.List.List_Iterator;
      use type Token_ID;

      --  [dragon] algorithm 4.9 pg 231; figure 4.38 pg 232; procedure "items"

      C : Item_Set_List := -- result
        (Head             => new Item_Set'
           (Closure
              ((Set       => New_Item_Node
                  (Production.List.Current (Production.List.First (Grammar)),
                   Production.List.RHS (Production.List.First (Grammar)).Tokens.First,
                   First_State_Index,
                   To_Lookahead (Descriptor, Descriptor.EOF_ID)),
                Goto_List => null,
                State     => First_State_Index,
                Next      => null),
               Has_Empty_Production, First, Grammar, Descriptor,
               Trace      => False)),
         Size             => 1);

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
               Put (Descriptor, I.all, Show_Lookaheads => True, Show_Goto_List => True);
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
                             ("  adding goto on " & Image (Descriptor, Symbol) & " to state" &
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
      Has_Empty_Production : in     Token_ID_Set;
      Conflicts            :    out Conflict_Lists.List;
      Table                : in out Parse_Table;
      Trace                : in     Boolean;
      Descriptor           : in FastToken.Descriptor)
   is
      --  Add actions for all Item_Sets to Table.

      Item_Set : LR1_Items.Item_Set_Ptr := Item_Sets.Head;
      use type LR1_Items.Item_Set_Ptr;
   begin
      while Item_Set /= null loop
         Add_Actions (Item_Set.all, Table, Has_Empty_Production, Conflicts, Trace, Descriptor);
         Item_Set := Item_Set.Next;
      end loop;

      if Trace then
         Ada.Text_IO.New_Line;
      end if;
   end Add_Actions;

   procedure Put_Parse_Table
     (Table      : in Parse_Table_Ptr;
      Item_Sets  : in LR1_Items.Item_Set_List;
      Descriptor : in FastToken.Descriptor)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("LR1 Parse Table:");
      Put_Line ("Panic_Recover:");
      Put (Descriptor, Table.Panic_Recover);
      New_Line;

      Put_Line ("Follow:");
      Put (Descriptor, Table.Follow);
      New_Line;

      for State in Table.States'Range loop
         LR1_Items.Put
           (Descriptor, LR1_Items.Find (State, Item_Sets).all, Kernel_Only => True, Show_Lookaheads => True);
         New_Line;
         Put (Descriptor, Table.States (State));

         New_Line;
      end loop;
   end Put_Parse_Table;

   function Check_Unused_Tokens
     (Descriptor : in FastToken.Descriptor;
      Grammar    : in Production.List.Instance)
     return Boolean
   is
      use Production.List;

      Used_Tokens : Token_ID_Set := (Descriptor.First_Terminal .. Descriptor.Last_Nonterminal => False);

      Unused_Tokens : Boolean := False;

      I : List_Iterator := First (Grammar);
   begin
      Used_Tokens (Descriptor.Accept_ID) := True;

      loop
         exit when Is_Done (I);
         declare
            use Production;
            use Token.List;
            Prod : constant Production.Instance := Current (I);
            J    : Token.List.List_Iterator     := First (Prod.RHS.Tokens);
         begin
            loop
               exit when Is_Done (J);
               Used_Tokens (ID (J)) := True;
               Next (J);
            end loop;
         end;

         Next (I);
      end loop;

      for I in Used_Tokens'Range loop
         if not Used_Tokens (I) then
            if not Unused_Tokens then
               Ada.Text_IO.Put_Line ("Unused tokens:");
               Unused_Tokens := True;
            end if;
            Ada.Text_IO.Put_Line (Image (Descriptor, I));
         end if;
      end loop;

      return Unused_Tokens;
   end Check_Unused_Tokens;

   function Generate
     (Grammar                  : in Production.List.Instance;
      Descriptor               : in FastToken.Descriptor;
      First_State_Index        : in State_Index;
      Known_Conflicts          : in Conflict_Lists.List := Conflict_Lists.Empty_List;
      Panic_Recover            : in Token_ID_Set        := Default_Panic_Recover;
      Trace                    : in Boolean             := False;
      Put_Parse_Table          : in Boolean             := False;
      Ignore_Unused_Tokens     : in Boolean             := False;
      Ignore_Unknown_Conflicts : in Boolean             := False)
     return Parse_Table_Ptr
   is
      use type Ada.Containers.Count_Type;

      Unused_Tokens : constant Boolean := Check_Unused_Tokens (Descriptor, Grammar);

      Table : Parse_Table_Ptr;

      Has_Empty_Production : constant Token_ID_Set          := LR1_Items.Has_Empty_Production (Grammar, Descriptor);
      First                : constant Token_Array_Token_Set := LR1_Items.First
        (Grammar, Descriptor, Has_Empty_Production, Trace);

      Item_Sets : constant LR1_Items.Item_Set_List := LR1_Item_Sets
        (Has_Empty_Production, First, Grammar, First_State_Index, Descriptor, Trace);

      Unknown_Conflicts    : Conflict_Lists.List;
      Known_Conflicts_Edit : Conflict_Lists.List := Known_Conflicts;
   begin
      if Trace then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("LR(1) Item_Sets:");
         LR1_Items.Put (Descriptor, Item_Sets);
      end if;

      Table := new Parse_Table
        (State_First       => First_State_Index,
         State_Last        => Item_Sets.Size - 1 + First_State_Index,
         First_Terminal    => Descriptor.First_Terminal,
         Last_Terminal     => Descriptor.Last_Terminal,
         First_Nonterminal => Descriptor.First_Nonterminal,
         Last_Nonterminal  => Descriptor.Last_Nonterminal);

      Table.Panic_Recover :=
        (if Panic_Recover = Default_Panic_Recover
         then (Table.First_Nonterminal .. Table.Last_Nonterminal => False)
         else Panic_Recover);

      Table.Follow := LR1_Items.Follow (Grammar, Descriptor, First, Has_Empty_Production);

      Add_Actions (Item_Sets, Has_Empty_Production, Unknown_Conflicts, Table.all, Trace, Descriptor);

      if Put_Parse_Table then
         LR1_Generator.Put_Parse_Table (Table, Item_Sets, Descriptor);
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

end FastToken.Parser.LR.LR1_Generator;
