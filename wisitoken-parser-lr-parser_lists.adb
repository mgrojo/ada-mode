--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014-2017  All Rights Reserved.
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

with Ada.Characters.Handling;
package body WisiToken.Parser.LR.Parser_Lists is

   procedure Pend (State : in out Parser_State; Item : in Pend_Item; Trace : in out WisiToken.Trace'Class)
   is begin
      State.Pend_Items.Put (Item);
      if Trace_Parse > 2 then
         Trace.Put (Integer'Image (State.Label) & ": pending ");
         Parser_Lists.Put (Trace, Item);
         Trace.New_Line;
      end if;
   end Pend;

   function New_List
     (First_State_Index  : in State_Index;
      First_Parser_Label : in Natural)
     return List
   is
      Stack : Parser_Stacks.Stack_Type;
   begin
      Stack.Push ((First_State_Index, Invalid_Token_ID));

      return Result : List
      do
         Result.Parser_Label := First_Parser_Label;

         Result.Elements.Append
           ((Current_Token            => Invalid_Token_ID,
             Current_Token_Is_Virtual => False,
             Last_Shift_Was_Virtual   => False,
             Stack                    => Stack,
             Pend_Items               => Pend_Items_Queues.Empty_Queue,
             Recover                  => Default_McKenzie,
             Local_Lookahead          => Token_Queues.Empty_Queue,
             Shared_Lookahead_Index   => SAL.Peek_Type'First,
             Zombie_Token_Count       => 0,
             Label                    => First_Parser_Label,
             Verb                     => Shift,
             Prev_Verb                => Parse_Action_Verbs'First,
             Pre_Reduce_Item          => Default_Parser_Stack_Item));
      end return;
   end New_List;

   function Count (List : in Parser_Lists.List) return Ada.Containers.Count_Type
   is begin
      return List.Elements.Length;
   end Count;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor
   is begin
      return (Elements => List.Elements'Access, Ptr => List.Elements.First);
   end First;

   procedure Next (Cursor : in out Parser_Lists.Cursor)
   is begin
      Parser_State_Lists.Next (Cursor.Ptr);
   end Next;

   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean
   is
      use Parser_State_Lists;
   begin
      return Cursor.Ptr = No_Element;
   end Is_Done;

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return Ada.Containers.Count_Type
   is begin
      return Cursor.Elements.Length;
   end Active_Parser_Count;

   function Label (Cursor : in Parser_Lists.Cursor) return Natural
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Ptr).Label;
   end Label;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in All_Parse_Action_Verbs)
   is begin
      Parser_State_Lists.Reference (Cursor.Ptr).Verb := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return All_Parse_Action_Verbs
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Ptr).Verb;
   end Verb;

   procedure Save_Verb (Cursor : in Parser_Lists.Cursor)
   is
      Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Ptr);
   begin
      Item.Prev_Verb := Item.Verb;
   end Save_Verb;

   function Prev_Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Ptr).Prev_Verb;
   end Prev_Verb;

   function State_Ref (Position : in Cursor) return State_Reference
   is begin
      return (Element => Parser_State_Lists.Constant_Reference (Position.Ptr).Element);
   end State_Ref;

   function State_Ref_2
     (Container : not null access List'Class;
      Label     : in              Natural)
     return State_Reference
   is
      use Parser_State_Lists;
      Ptr : Parser_State_Lists.Cursor := Container.Elements.First;
   begin
      loop
         exit when Constant_Reference (Ptr).Label = Label;
         Next (Ptr);
      end loop;
      return (Element => Reference (Ptr).Element);
   end State_Ref_2;

   function McKenzie_Ref (Position : in Cursor) return McKenzie_Access
   is begin
      return Parser_State_Lists.Persistent_Ref (Position.Ptr).Recover'Access;
   end McKenzie_Ref;

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      use all type SAL.Base_Peek_Type;
      Item  : Parser_State renames Parser_State_Lists.Constant_Reference (Cursor.Ptr);
      Stack : Parser_Stacks.Stack_Type renames Item.Stack;
   begin
      Trace.Put (Natural'Image (Item.Label) & " stack: ");
      Put_Top_10 (Trace, Stack);
   end Put_Top_10;

   procedure Pre_Reduce_Stack_Save (Cursor : in Parser_Lists.Cursor)
   is
      Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Ptr);
   begin
      Item.Pre_Reduce_Item := Item.Stack.Peek;
   end Pre_Reduce_Stack_Save;

   procedure Prepend_Copy
     (List   : in out Parser_Lists.List;
      Cursor : in     Parser_Lists.Cursor'Class)
   is
      New_Item : Parser_State;
   begin
      List.Parser_Label := List.Parser_Label + 1;
      declare
         Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Ptr).Element.all;
         --  We can't do 'Prepend' in the scope of this 'renames';
         --  that would be tampering with cursors.
      begin
         New_Item :=
           (Current_Token            => Item.Current_Token,
            Current_Token_Is_Virtual => Item.Current_Token_Is_Virtual,
            Last_Shift_Was_Virtual   => Item.Last_Shift_Was_Virtual,
            Stack                    => Item.Stack,
            Pend_Items               => Item.Pend_Items,
            Recover                  => Default_McKenzie,
            Local_Lookahead          => Item.Local_Lookahead,
            Shared_Lookahead_Index   => Item.Shared_Lookahead_Index,
            Zombie_Token_Count       => 0,
            Label                    => List.Parser_Label,
            Verb                     => Item.Verb,
            Prev_Verb                => Item.Prev_Verb,
            Pre_Reduce_Item          => Item.Pre_Reduce_Item);
      end;
      List.Elements.Prepend (New_Item);
   end Prepend_Copy;

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class)
   is
      Temp : Parser_State_Lists.Cursor := Cursor.Ptr;
   begin
      Parser_State_Lists.Next (Cursor.Ptr);
      Parser_State_Lists.Delete (Cursor.Elements.all, Temp);
   end Free;

   ----------
   --  stuff for iterators

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor
   is begin
      return (Ptr.Elements, Ptr.Ptr);
   end To_Cursor;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  :         in Parser_Node_Access)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return
        (Element => Parser_State_Lists.Constant_Reference (Position.Ptr).Element,
         Dummy   => 1);
   end Constant_Reference;

   function Reference
     (Container : aliased in out List'Class;
      Position  :         in     Parser_Node_Access)
     return State_Reference
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Parser_State_Lists.Reference (Position.Ptr).Element);
   end Reference;

   type List_Access is access all List;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container : List_Access;
   end record;

   overriding function First (Object : Iterator) return Parser_Node_Access;
   overriding function Next
     (Object   : in Iterator;
      Position : in Parser_Node_Access)
     return Parser_Node_Access;

   overriding function First (Object : Iterator) return Parser_Node_Access
   is begin
      return (Elements => Object.Container.Elements'Access, Ptr => Object.Container.Elements.First);
   end First;

   overriding function Next
     (Object   : in Iterator;
      Position : in Parser_Node_Access)
     return Parser_Node_Access
   is
      pragma Unreferenced (Object);
   begin
      return (Position.Elements, Parser_State_Lists.Next (Position.Ptr));
   end Next;

   function Iterate (Container : aliased in out List) return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   function Has_Element (Iterator : in Parser_Node_Access) return Boolean
   is begin
      return Parser_State_Lists.Has_Element (Iterator.Ptr);
   end Has_Element;

   function Label (Iterator : in Parser_State) return Natural
   is begin
      return Iterator.Label;
   end Label;

   function Verb (Iterator : in Parser_State) return All_Parse_Action_Verbs
   is begin
      return Iterator.Verb;
   end Verb;

   procedure Set_Verb (Iterator : in out Parser_State; Verb : in All_Parse_Action_Verbs)
   is begin
      Iterator.Verb := Verb;
   end Set_Verb;

   function Pre_Reduce_Stack_Item (Iterator : in Parser_State) return Parser_Stack_Item
   is begin
      return Iterator.Pre_Reduce_Item;
   end Pre_Reduce_Stack_Item;

   procedure Put_Top_10 (Iterator : in Parser_State; Trace : in out WisiToken.Trace'Class)
   is begin
      Trace.Put (Natural'Image (Iterator.Label) & " stack: ");
      Put_Top_10 (Trace, Iterator.Stack);
   end Put_Top_10;

   ----------
   --  For unit tests

   procedure Put (Trace : in out WisiToken.Trace'Class; Pend_Item : in Parser_Lists.Pend_Item)
   is
      use Ada.Characters.Handling;
   begin
      Trace.Put (Pend_Semantic_Verbs'Image (Pend_Item.Verb) & " ");

      case Pend_Item.Verb is
      when Virtual_To_Lookahead .. Push_Current =>
         Trace.Put (Image (Trace.Descriptor.all, Pend_Item.ID));

      when Discard_Lookahead .. Discard_Stack =>
         Trace.Put (Image (Trace.Descriptor.all, Pend_Item.Discard_ID));

      when Reduce_Stack =>
         declare
            Action_Name : constant String := To_Lower
              (Image (Trace.Descriptor.all, Pend_Item.Action.LHS)) &
              "_" & WisiToken.Int_Image (Pend_Item.Action.Index);
         begin
            Trace.Put
              (Action_Name & ": " & Image (Trace.Descriptor.all, Pend_Item.Action.LHS) & " <= ");
            Put (Trace, Pend_Item.Tokens);
         end;

      when Recover =>
         null;
      end case;
   end Put;

   procedure Put_Pending_Actions (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      Queue : Pend_Items_Queues.Queue_Type renames Parser_State_Lists.Constant_Reference
        (Cursor.Ptr).Pend_Items;
   begin
      for I in 1 .. Queue.Count loop
         declare
            Item : Pend_Item renames Queue.Peek (I);
         begin
            Put (Trace, Item);
            Trace.New_Line;
         end;
      end loop;
   end Put_Pending_Actions;

end WisiToken.Parser.LR.Parser_Lists;
