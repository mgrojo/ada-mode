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

   function New_List
     (First_State_Index  : in State_Index;
      First_Parser_Label : in Integer)
     return List
   is
      Stack : Parse_Stacks.Stack_Type;
   begin
      Stack.Push ((First_State_Index, Invalid_Token));

      return Result : List
      do
         Result.Parser_Label := First_Parser_Label;

         Result.Elements.Append
           ((Label           => First_Parser_Label,
             Verb            => Parse_Action_Verbs'First,
             Prev_Verb       => Parse_Action_Verbs'First,
             Stack           => Stack,
             Pre_Reduce_Item => Default_Parse_Stack_Item,
             Pending_Actions => Pend_Items_Queues.Empty_Queue,
             Recover         => null));
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

   function Label (Cursor : in Parser_Lists.Cursor) return Integer
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Label;
   end Label;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs)
   is
      Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr);
   begin
      Item.Prev_Verb := Item.Verb;
      Item.Verb      := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Verb;
   end Verb;

   function Prev_Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Prev_Verb;
   end Prev_Verb;

   procedure Set_Recover (Cursor : in Parser_Lists.Cursor; Data : in Recover_Data_Access)
   is
      Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr);
   begin
      Free (Item.Recover);
      Item.Recover := Data;
   end Set_Recover;

   function Recover_Ref (Position : in Cursor) return Recover_Reference
   is begin
      return (Element => Parser_State_Lists.Constant_Reference (Position.Elements.all, Position.Ptr).Recover);
   end Recover_Ref;

   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Stack.Is_Empty;
   end Stack_Empty;

   function Copy_Stack (Cursor : in Parser_Lists.Cursor) return Parse_Stacks.Stack_Type
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Stack;
   end Copy_Stack;

   function Peek (Cursor : in Parser_Lists.Cursor; Depth : in SAL.Base_Peek_Type := 1) return Parse_Stack_Item
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Stack.Peek (Depth);
   end Peek;

   function Pop (Cursor : in Parser_Lists.Cursor) return Parse_Stack_Item
   is begin
      return Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr).Stack.Pop;
   end Pop;

   procedure Pop (Cursor : in Parser_Lists.Cursor)
   is begin
      Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr).Stack.Pop;
   end Pop;

   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Parse_Stack_Item)
   is begin
      Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr).Stack.Push (Item);
   end Push;

   function Stack_Equal (Cursor_1, Cursor_2 : in Parser_Lists.Cursor) return Boolean
   is
      use all type Parse_Stacks.Stack_Type;
   begin
      return Parser_State_Lists.Constant_Reference (Cursor_1.Elements.all, Cursor_1.Ptr).Stack =
        Parser_State_Lists.Constant_Reference (Cursor_2.Elements.all, Cursor_2.Ptr).Stack;
   end Stack_Equal;

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      use all type SAL.Base_Peek_Type;
      Item  : Parser_State renames Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr);
      Stack : Parse_Stacks.Stack_Type renames Item.Stack;
      Last  : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type'Min (10, Stack.Depth);
   begin
      Trace.Put (Integer'Image (Item.Label) & " stack: ");
      for I in 1 .. Last loop
         Trace.Put
           (State_Index'Image (Stack.Peek (I).State) & " : " &
              (if I = Last
               then ""
               else Image (Trace.Descriptor.all, Stack.Peek (I).Token) & ", "));
      end loop;
      Trace.New_Line;
   end Put_Top_10;

   procedure Pre_Reduce_Stack_Save (Cursor : in Parser_Lists.Cursor)
   is
      Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr);
   begin
      Item.Pre_Reduce_Item := Item.Stack.Peek;
   end Pre_Reduce_Stack_Save;

   function Pre_Reduce_Stack_Item (Cursor : in Parser_Lists.Cursor) return Parse_Stack_Item
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Pre_Reduce_Item;
   end Pre_Reduce_Stack_Item;

   function Pending_Actions_Count (Cursor : in Parser_Lists.Cursor) return SAL.Base_Peek_Type
   is begin
      return Parser_State_Lists.Constant_Reference (Cursor.Elements.all, Cursor.Ptr).Pending_Actions.Count;
   end Pending_Actions_Count;

   procedure Enqueue
     (Cursor       : in Parser_Lists.Cursor;
      Action_Token : in Parser_Lists.Action_Token)
   is
      Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr);
   begin
      Item.Pending_Actions.Put (Action_Token);
   end Enqueue;

   function Dequeue (Cursor : in Parser_Lists.Cursor) return Action_Token
   is begin
      return Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr).Pending_Actions.Get;
   end Dequeue;

   function Pending_Actions_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr).Pending_Actions.Is_Empty;
   end Pending_Actions_Empty;

   procedure Prepend_Copy
     (List   : in out Parser_Lists.List;
      Cursor : in     Parser_Lists.Cursor'Class)
   is
      New_Item : Parser_State;
   begin
      List.Parser_Label := List.Parser_Label + 1;
      declare
         Item : Parser_State renames Parser_State_Lists.Reference (Cursor.Elements.all, Cursor.Ptr).Element.all;
         --  We can't do 'Prepend' in the scope of this 'renames';
         --  that would be tampering with cursors.
      begin
         New_Item :=
           (Label           => List.Parser_Label,
            Verb            => Item.Verb,
            Prev_Verb       => Item.Prev_Verb,
            Stack           => Item.Stack,
            Pre_Reduce_Item => Item.Pre_Reduce_Item,
            Pending_Actions => Item.Pending_Actions,
            Recover         => null);
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

   procedure Free (List : in out Parser_Lists.List)
   is begin
      Parser_State_Lists.Clear (List.Elements);
      List.Parser_Label := -1;
   end Free;

   ----------
   --  stuff for iterators

   function To_Cursor (Ptr : in Parser_Node_Access) return Cursor
   is begin
      return (Ptr.Elements, Ptr.Ptr);
   end To_Cursor;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  : in Parser_Node_Access)
     return Constant_Reference_Type
   is begin
      return (Element => Parser_State_Lists.Constant_Reference (Container.Elements, Position.Ptr).Element);
   end Constant_Reference;

   type List_Access is access all List;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container : List_Access;
   end record;

   overriding function First (Object : Iterator) return Parser_Node_Access;
   overriding function Next
     (Object   : Iterator;
      Position : Parser_Node_Access)
     return Parser_Node_Access;

   overriding function First (Object : Iterator) return Parser_Node_Access
   is begin
      return (Elements => Object.Container.Elements'Access, Ptr => Object.Container.Elements.First);
   end First;

   overriding function Next
     (Object   : Iterator;
      Position : Parser_Node_Access)
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

   function Verb (Iterator : in Parser_Node_Access) return Parse_Action_Verbs
   is begin
      return Parser_State_Lists.Constant_Reference (Iterator.Elements.all, Iterator.Ptr).Verb;
   end Verb;

   ----------
   --  For unit tests

   procedure Put (Trace : in out WisiToken.Trace'Class; Action_Token : in Parser_Lists.Action_Token)
   is
      use Ada.Characters.Handling;
   begin
      case Action_Token.Action.Verb is
      when Shift =>
         Trace.Put
           ("shift " &
              Image (Trace.Descriptor.all, Token.List.Current (Token.List.First (Action_Token.Tokens))));

      when Reduce =>
         declare
            Action_Name : constant String := To_Lower
              (Image (Trace.Descriptor.all, Action_Token.Action.LHS)) &
              "_" & WisiToken.Int_Image (Action_Token.Action.Index);
         begin
            Trace.Put
              (Action_Name & ": " &
                 Image (Trace.Descriptor.all, Action_Token.Action.LHS) & " <= ");
            Token.List.Put (Trace, Action_Token.Tokens);
         end;

      when Accept_It | Error =>
         raise Programmer_Error;
      end case;
   end Put;

   procedure Put_Pending_Actions (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      Queue : Pend_Items_Queues.Queue_Type renames Parser_State_Lists.Constant_Reference
        (Cursor.Elements.all, Cursor.Ptr).Pending_Actions;
   begin
      for I in 1 .. Queue.Count loop
         declare
            Item : Action_Token renames Queue.Peek (I);
         begin
            Put (Trace, Item);
            Trace.New_Line;
         end;
      end loop;
   end Put_Pending_Actions;

end WisiToken.Parser.LR.Parser_Lists;
