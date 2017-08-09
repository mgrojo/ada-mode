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

      return
        (Parser_Label          => First_Parser_Label,
         Head                  => new Parser_Node'
           (Item               =>
              (Label           => First_Parser_Label,
               Verb            => Parse_Action_Verbs'First,
               Prev_Verb       => Parse_Action_Verbs'First,
               Stack           => Stack,
               Pre_Reduce_Item => Default_Parse_Stack_Item,
               Pending_Actions => (null, null),
               Recover         => null),
            Next               => null,
            Prev               => null),
         Parser_Free           => null,
         Action_Token_Free     => null,
         Count                 => 1);
   end New_List;

   function Count (List : in Parser_Lists.List) return Integer
   is begin
      return List.Count;
   end Count;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor
   is begin
      return (List'Access, Ptr => List.Head);
   end First;

   procedure Next (Cursor : in out Parser_Lists.Cursor)
   is begin
      if Cursor.Ptr /= null then
         Cursor.Ptr := Cursor.Ptr.Next;
      end if;
   end Next;

   function Is_Done (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr = null;
   end Is_Done;

   function Active_Parser_Count (Cursor : in Parser_Lists.Cursor) return Integer
   is begin
      return Cursor.List.Count;
   end Active_Parser_Count;

   function Label (Cursor : in Parser_Lists.Cursor) return Integer
   is begin
      return Cursor.Ptr.Item.Label;
   end Label;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs)
   is begin
      Cursor.Ptr.Item.Prev_Verb := Cursor.Ptr.Item.Verb;

      Cursor.Ptr.Item.Verb := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Cursor.Ptr.Item.Verb;
   end Verb;

   function Prev_Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Cursor.Ptr.Item.Prev_Verb;
   end Prev_Verb;

   procedure Set_Recover (Cursor : in Parser_Lists.Cursor; Data : in Recover_Data_Access)
   is begin
      Free (Cursor.Ptr.Item.Recover);
      Cursor.Ptr.Item.Recover := Data;
   end Set_Recover;

   function Recover_Ref (Position : in Cursor) return Recover_Reference
   is begin
      return (Element => Position.Ptr.all.Item.Recover);
   end Recover_Ref;

   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr.Item.Stack.Is_Empty;
   end Stack_Empty;

   function Copy_Stack (Cursor : in Parser_Lists.Cursor) return Parse_Stacks.Stack_Type
   is begin
      return Cursor.Ptr.Item.Stack;
   end Copy_Stack;

   function Peek (Cursor : in Parser_Lists.Cursor; Depth : in Integer := 1) return Parse_Stack_Item
   is begin
      return Cursor.Ptr.Item.Stack.Peek (SAL.Base_Peek_Type (Depth));
   end Peek;

   function Pop (Cursor : in Parser_Lists.Cursor) return Parse_Stack_Item
   is begin
      return Cursor.Ptr.Item.Stack.Pop;
   end Pop;

   procedure Pop (Cursor : in Parser_Lists.Cursor)
   is begin
      Cursor.Ptr.Item.Stack.Pop;
   end Pop;

   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Parse_Stack_Item)
   is begin
      Cursor.Ptr.Item.Stack.Push (Item);
   end Push;

   function Stack_Equal (Cursor_1, Cursor_2 : in Parser_Lists.Cursor) return Boolean
   is
      use all type Parse_Stacks.Stack_Type;
   begin
      return Cursor_1.Ptr.Item.Stack = Cursor_2.Ptr.Item.Stack;
   end Stack_Equal;

   procedure Put_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      use all type SAL.Base_Peek_Type;
      Stack : Parse_Stacks.Stack_Type renames Cursor.Ptr.Item.Stack;
      Last  : constant SAL.Base_Peek_Type := SAL.Base_Peek_Type'Min (10, Stack.Depth);
   begin
      Trace.Put (Integer'Image (Cursor.Ptr.Item.Label) & " stack: ");
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
   is begin
      Cursor.Ptr.Item.Pre_Reduce_Item := Cursor.Ptr.Item.Stack.Peek;
   end Pre_Reduce_Stack_Save;

   function Pre_Reduce_Stack_Item (Cursor : in Parser_Lists.Cursor) return Parse_Stack_Item
   is begin
      return Cursor.Ptr.Item.Pre_Reduce_Item;
   end Pre_Reduce_Stack_Item;

   function Pending_Actions_Count (Cursor : in Parser_Lists.Cursor) return Integer
   is
      Pending_Actions : Action_Token_Node_Access := Cursor.Ptr.Item.Pending_Actions.Head;
      Result       : Integer                  := 0;
   begin
      loop
         exit when Pending_Actions = null;
         Result := Result + 1;
         Pending_Actions := Pending_Actions.Next;
      end loop;
      return Result;
   end Pending_Actions_Count;

   procedure Enqueue
     (List              : in out Action_Token_List;
      Action_Token_Free : in out Action_Token_Node_Access;
      Action_Token      : in     Parser_Lists.Action_Token)
   is
      Temp : constant Action_Token_Node_Access := Action_Token_Free;

      Node : constant Action_Token_Node := (Action_Token, null, List.Tail);
   begin
      if Temp = null then
         if List.Tail = null then
            List.Tail := new Action_Token_Node'(Node);
            List.Head := List.Tail;
         else
            List.Tail := new Action_Token_Node'(Node);
         end if;
      else
         Action_Token_Free := Action_Token_Free.Next;
         Temp.all          := Node;
         if List.Tail = null then
            List.Tail := Temp;
            List.Head := List.Tail;
         else
            List.Tail := Temp;
         end if;
      end if;

      if List.Tail.Prev /= null then
         List.Tail.Prev.Next := List.Tail;
      end if;
   end Enqueue;

   procedure Enqueue
     (Cursor       : in Parser_Lists.Cursor;
      Action_Token : in Parser_Lists.Action_Token)
   is begin
      Enqueue (Cursor.Ptr.Item.Pending_Actions, Cursor.List.Action_Token_Free, Action_Token);
   end Enqueue;

   procedure Free (List : in out Parser_Lists.List; Action_Token : in out Action_Token_Node_Access)
   is
      Temp_Free : constant Action_Token_Node_Access := List.Action_Token_Free;
   begin
      List.Action_Token_Free := Action_Token;
      Action_Token           := Action_Token.Next; -- not null; for free (cursor) below

      List.Action_Token_Free.all :=
        (Item         => Null_Action_Token, -- New_Token, Tokens are free'd after being passed to user action
         Next         => Temp_Free,
         Prev         => null);
   end Free;

   function Dequeue (Cursor : in Parser_Lists.Cursor) return Action_Token
   is
      Result : constant Action_Token := Cursor.Ptr.Item.Pending_Actions.Head.Item;
   begin
      Free (Cursor.List.all, Cursor.Ptr.Item.Pending_Actions.Head);

      if Cursor.Ptr.Item.Pending_Actions.Head = null then
         Cursor.Ptr.Item.Pending_Actions.Tail := null;
      end if;

      return Result;
   end Dequeue;

   function Pending_Actions_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr.Item.Pending_Actions.Head = null;
   end Pending_Actions_Empty;

   procedure Deep_Copy
     (Pending_Actions     : in     Action_Token_List;
      Action_Token_Free   : in out Action_Token_Node_Access;
      New_Pending_Actions :    out Action_Token_List)
   is
      use Token.List;

      J          : Action_Token_Node_Access := Pending_Actions.Head;
      Iter       : List_Iterator;
      New_Tokens : Token.List.Instance;
   begin

      J := Pending_Actions.Head;
      loop
         exit when J = null;

         Iter       := J.Item.Tokens.First;
         New_Tokens := Null_List;
         loop
            exit when Iter = Null_Iterator;
            Append (New_Tokens, Current (Iter));
            Next (Iter);
         end loop;

         Enqueue
           (New_Pending_Actions,
            Action_Token_Free,
            (J.Item.Action, New_Tokens));

         J := J.Next;
      end loop;
   end Deep_Copy;

   procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class)
   is
      Temp : constant Parser_Node_Access := List.Parser_Free;

      New_Action_Token : Action_Token_List;

      New_Parser : Parser_Node;
   begin
      Deep_Copy (Cursor.Ptr.Item.Pending_Actions, List.Action_Token_Free, New_Action_Token);

      New_Parser :=
        (Item               =>
           (Label           => List.Parser_Label + 1,
            Verb            => Cursor.Ptr.Item.Verb,
            Prev_Verb       => Cursor.Ptr.Item.Prev_Verb,
            Stack           => Cursor.Ptr.Item.Stack,
            Pre_Reduce_Item => Cursor.Ptr.Item.Pre_Reduce_Item,
            Pending_Actions => New_Action_Token,
            Recover         => null),
         Next               => List.Head,
         Prev               => null);

      List.Parser_Label := List.Parser_Label + 1;
      List.Count        := List.Count + 1;

      if Temp = null then
         List.Head := new Parser_Node'(New_Parser);

         List.Head.Next.Prev := List.Head;
      else
         List.Parser_Free    := List.Parser_Free.Next;
         Temp.all            := New_Parser;
         List.Head           := Temp;
         List.Head.Next.Prev := List.Head;
      end if;
   end Prepend_Copy;

   procedure Free (Cursor : in out Parser_Lists.Cursor'Class)
   is
      Temp_Free    : constant Parser_Node_Access := Cursor.List.Parser_Free;
      Action_Token : Action_Token_Node_Access    := Cursor.Ptr.Item.Pending_Actions.Head;
   begin
      Cursor.List.Count := Cursor.List.Count - 1;

      if Cursor.List.Head = Cursor.Ptr then
         Cursor.List.Head := Cursor.Ptr.Next;
      end if;

      if Cursor.Ptr.Prev /= null then
         Cursor.Ptr.Prev.Next := Cursor.Ptr.Next;
      end if;

      if Cursor.Ptr.Next /= null then
         Cursor.Ptr.Next.Prev := Cursor.Ptr.Prev;
      end if;

      Cursor.List.Parser_Free := Cursor.Ptr;
      Cursor.Ptr              := Cursor.Ptr.Next;

      Cursor.List.Parser_Free.Next := Temp_Free;
      Cursor.List.Parser_Free.Prev := null;

      loop
         exit when Action_Token = null;
         Free (Cursor.List.all, Action_Token);
      end loop;
   end Free;

   ----------
   --  stuff for iterators

   function To_Cursor
     (List : aliased in out Parser_Lists.List'Class;
      Ptr  :         in     Parser_Node_Access)
     return Cursor
   is begin
      return (List'Access, Ptr);
   end To_Cursor;

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  : in Parser_Node_Access)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      --  WORKAROUND: gcc 6 reports an error for Position.Item'Access here; this passes all tests
      return (Element => Position.all.Item'Access);
   end Constant_Reference;

   type List_Access_Constant is access constant List;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container : List_Access_Constant;
   end record;

   overriding function First (Object : Iterator) return Parser_Node_Access;
   overriding function Next
     (Object   : Iterator;
      Position : Parser_Node_Access)
     return Parser_Node_Access;

   overriding function First (Object : Iterator) return Parser_Node_Access
   is begin
      return Object.Container.Head;
   end First;

   overriding function Next
     (Object   : Iterator;
      Position : Parser_Node_Access)
     return Parser_Node_Access
   is
      pragma Unreferenced (Object);
   begin
      if Position = null then
         return null;
      else
         return Position.Next;
      end if;
   end Next;

   function Has_Element (Cursor : in Parser_Node_Access) return Boolean
   is begin
      return Cursor /= null;
   end Has_Element;

   function Verb (Cursor : in Parser_Node_Access) return Parse_Action_Verbs
   is begin
      return Cursor.Item.Verb;
   end Verb;

   function Iterate (Container : aliased List) return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   function Count (Action_Token : in Action_Token_List) return Integer
   is
      Result : Integer := 0;
      I : Action_Token_Node_Access := Action_Token.Head;
   begin
      loop
         exit when I = null;
         Result := Result + 1;
         I      := I.Next;
      end loop;
      return Result;
   end Count;

   ----------
   --  For unit tests

   function Parser_Free_Count (List : in Parser_Lists.List) return Integer
   is
      Result : Integer := 0;
      Node   : Parser_Node_Access := List.Parser_Free;
   begin
      loop
         exit when Node = null;
         Result := Result + 1;
         Node   := Node.Next;
      end loop;
      return Result;
   end Parser_Free_Count;

   function Action_Token_Free_Count (List : in Parser_Lists.List) return Integer
   is
      Result : Integer := 0;
      Node   : Action_Token_Node_Access := List.Action_Token_Free;
   begin
      loop
         exit when Node = null;
         Result := Result + 1;
         Node   := Node.Next;
      end loop;
      return Result;
   end Action_Token_Free_Count;

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
      Action_Token : Action_Token_Node_Access := Cursor.Ptr.Item.Pending_Actions.Head;
   begin
      loop
         exit when Action_Token = null;
         Put (Trace, Action_Token.Item);
         Trace.New_Line;
         Action_Token := Action_Token.Next;
      end loop;
   end Put_Pending_Actions;

end WisiToken.Parser.LR.Parser_Lists;
