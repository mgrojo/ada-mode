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
   is begin
      --  FIXME: should clear all panic lists in existing parser; use Controlled.
      return
        (Parser_Label          => First_Parser_Label,
         Head                  => new Parser_Node'
           (Item               =>
              (Label           => First_Parser_Label,
               Verb            => Parse_Action_Verbs'First,
               Stack           => new Stack_Node'
                 (Item         =>
                    (State     => First_State_Index,
                     Token     => Invalid_Token),
                  Next         => null),
               Pending_Actions => (null, null),
               Panic           => Default_Panic),
            Next               => null,
            Prev               => null),
         Parser_Free           => null,
         Stack_Free            => null,
         Action_Token_Free     => null,
         Count                 => 1);

   end New_List;

   function Count (List : in Parser_Lists.List) return Integer
   is begin
      return List.Count;
   end Count;

   function First (List : aliased in out Parser_Lists.List'Class) return Cursor
   is begin
      --  WORKAROUND: with 'Access, Debian gnat 4.9.2 reports
      --  "non-local pointer cannot point to local object", even
      --  though GNAT Pro 7.3.1 and GNAT GPL 2014 allow 'Access. There
      --  doesn't seem to be a way to use a legitimate access param
      --  while still meeting the Iterator requirements.
      return (List'Unchecked_Access, Ptr => List.Head);
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
      Cursor.Ptr.Item.Verb := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Cursor.Ptr.Item.Verb;
   end Verb;

   function Panic_Ref (Position : in Cursor) return Panic_Reference
   is begin
      return (Element => Position.Ptr.all.Item.Panic'Access);
   end Panic_Ref;

   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr.Item.Stack = null;
   end Stack_Empty;

   function Peek (Cursor : in Parser_Lists.Cursor; Depth : in Integer := 1) return Stack_Item
   is
      Ptr : Stack_Node_Access := Cursor.Ptr.Item.Stack;
   begin
      --  We don't check if Depth is > stack size; that's a severe programming error.
      for I in 2 .. Depth loop
         Ptr := Ptr.Next;
      end loop;
      return Ptr.Item;
   end Peek;

   procedure Free (List : in out Parser_Lists.List; Stack : in out Stack_Node_Access)
   is
      Temp_Free : constant Stack_Node_Access := List.Stack_Free;
   begin
      List.Stack_Free := Stack;
      Stack           := Stack.Next; -- not null; for free (cursor) below

      List.Stack_Free.all :=
        (Item     =>
           (State => Unknown_State,
            Token => Token_ID'First),
         Next     => Temp_Free);
   end Free;

   function Pop (Cursor : in Parser_Lists.Cursor) return Stack_Item
   is
      Result : constant Stack_Item := Cursor.Ptr.Item.Stack.Item;
   begin
      Free (Cursor.List.all, Cursor.Ptr.Item.Stack);

      return Result;
   end Pop;

   procedure Pop (Cursor : in Parser_Lists.Cursor)
   is begin
      Free (Cursor.List.all, Cursor.Ptr.Item.Stack);
   end Pop;

   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Stack_Item)
   is
      Temp : constant Stack_Node_Access := Cursor.List.Stack_Free;
   begin
      if Temp = null then
         Cursor.Ptr.Item.Stack := new Stack_Node'(Item, Cursor.Ptr.Item.Stack);
      else
         Cursor.List.Stack_Free := Cursor.List.Stack_Free.Next;
         Temp.all               := (Item, Cursor.Ptr.Item.Stack);
         Cursor.Ptr.Item.Stack  := Temp;
      end if;
   end Push;

   function Stack_Equal (Cursor_1, Cursor_2 : in Parser_Lists.Cursor) return Boolean
   is
      use type Token_ID;

      Stack_1 : Stack_Node_Access := Cursor_1.Ptr.Item.Stack;
      Stack_2 : Stack_Node_Access := Cursor_2.Ptr.Item.Stack;
   begin
      loop
         exit when Stack_1 = null or Stack_2 = null;
         if not
           (Stack_1.Item.State = Stack_2.Item.State and
              Stack_1.Item.Token = Stack_2.Item.Token)
         then
            return False;
         end if;
         Stack_1 := Stack_1.Next;
         Stack_2 := Stack_2.Next;
      end loop;
      return Stack_1 = null and Stack_2 = null;
   end Stack_Equal;

   procedure Put_Trace_Top_10 (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      Stack_I : Stack_Node_Access := Cursor.Ptr.Item.Stack;
   begin
      Trace.Put (Integer'Image (Cursor.Ptr.Item.Label) & " stack: ");
      for I in 1 .. 10 loop
         exit when Stack_I = null;
         Trace.Put
           (State_Index'Image (Stack_I.Item.State) & " : " &
              (if Stack_I.Next = null
               then ""
               else Image (Trace.Descriptor.all, Stack_I.Item.Token) & ", "));
         Stack_I := Stack_I.Next;
      end loop;
      Trace.New_Line;
   end Put_Trace_Top_10;

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
     (Stack               : in     Stack_Node_Access;
      Stack_Free          : in out Stack_Node_Access;
      Pending_Actions     : in     Action_Token_List;
      Action_Token_Free   : in out Action_Token_Node_Access;
      New_Stack           :    out Stack_Node_Access;
      New_Pending_Actions :    out Action_Token_List)
   is
      use Token.List;

      J          : Action_Token_Node_Access := Pending_Actions.Head;
      Iter       : List_Iterator;
      New_Tokens : Token.List.Instance;

      I    : Stack_Node_Access := Stack;
      Copy : Stack_Node_Access;
      Temp : Stack_Node_Access;

      New_Stack_Item   : Stack_Item;
   begin

      --  Create a copy of Stack in Copy, in reverse order.
      loop
         exit when I = null;

         New_Stack_Item := I.Item;

         if Stack_Free = null then
            Copy := new Stack_Node'(New_Stack_Item, Copy);
         else
            Temp       := Copy;
            Copy       := Stack_Free;
            Stack_Free := Stack_Free.Next;
            Copy.all   := (New_Stack_Item, Temp);
         end if;
         I := I.Next;
      end loop;

      --  Move to New_Stack, in correct order.
      I         := Copy;
      New_Stack := null;

      loop
         exit when I = null;
         Temp      := I.Next;
         I.Next    := New_Stack;
         New_Stack := I;
         I         := Temp;
      end loop;

      --  Copy Pending_Actions
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

      New_Stack        : Stack_Node_Access;
      New_Action_Token : Action_Token_List;

      New_Parser : Parser_Node;
   begin
      Deep_Copy
        (Cursor.Ptr.Item.Stack, List.Stack_Free, Cursor.Ptr.Item.Pending_Actions, List.Action_Token_Free,
         New_Stack, New_Action_Token);

      New_Parser :=
        (Item =>
           (List.Parser_Label + 1,
            Cursor.Ptr.Item.Verb,
            New_Stack,
            New_Action_Token,
            Default_Panic),
         Next => List.Head,
         Prev => null);

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
      Stack        : Stack_Node_Access           := Cursor.Ptr.Item.Stack;
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
         exit when Stack = null;
         Free (Cursor.List.all, Stack);
      end loop;

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
      --  see WORKAROUND in First
      return (List'Unchecked_Access, Ptr);
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
      --  see WORKAROUND in First
      return Iterator'(Container => Container'Unchecked_Access);
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

   function Stack_Free_Count (List : in Parser_Lists.List) return Integer
   is
      Result : Integer := 0;
      Node   : Stack_Node_Access := List.Stack_Free;
   begin
      loop
         exit when Node = null;
         Result := Result + 1;
         Node   := Node.Next;
      end loop;
      return Result;
   end Stack_Free_Count;

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

   procedure Put_Trace (Trace : in out WisiToken.Trace'Class; Action_Token : in Parser_Lists.Action_Token)
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
            Token.List.Put_Trace (Trace, Action_Token.Tokens);
         end;

      when Accept_It | Error =>
         raise Programmer_Error;
      end case;
   end Put_Trace;

   procedure Put_Trace_Pending_Actions (Trace : in out WisiToken.Trace'Class; Cursor : in Parser_Lists.Cursor)
   is
      Action_Token : Action_Token_Node_Access := Cursor.Ptr.Item.Pending_Actions.Head;
   begin
      loop
         exit when Action_Token = null;
         Put_Trace (Trace, Action_Token.Item);
         Trace.New_Line;
         Action_Token := Action_Token.Next;
      end loop;
   end Put_Trace_Pending_Actions;

end WisiToken.Parser.LR.Parser_Lists;
