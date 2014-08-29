--  Abstract :
--
--  see spec
--
--  Copyright (C) 2014  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);
separate (OpenToken.Production.Parser.LALR.Parser)
package body Parser_Lists is

   --  FIXME: move to List for unit tests
   Parser_Label : Integer;

   --  FIXME: procedure Free is new Ada.Unchecked_Deallocation (Stack_Node, Stack_Node_Access);
   --  FIXME: procedure Free is new Ada.Unchecked_Deallocation (Parser_State, Parser_State_Access);

   function Initialize return List
   is begin
      Parser_Label := 1;

      return
        (Head              => new Parser_State'
           (Label          => Parser_Label,
            Verb           => Parse_Action_Verbs'First,
            Stack          =>
              (Head        => new Stack_Node'
                 (Item     =>
                    (State => State_Index'First,
                     Token => null),
                  Next     => null)),
            Next           => null),
         Parser_Free       => null,
         Stack_Free        => null,
         Count             => 1);

   end Initialize;

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

   function Label (Cursor : in Parser_Lists.Cursor) return Integer
   is begin
      return Cursor.Ptr.Label;
   end Label;

   procedure Set_Verb (Cursor : in Parser_Lists.Cursor; Verb : in Parse_Action_Verbs)
   is begin
      Cursor.Ptr.Verb := Verb;
   end Set_Verb;

   function Verb (Cursor : in Parser_Lists.Cursor) return Parse_Action_Verbs
   is begin
      return Cursor.Ptr.Verb;
   end Verb;

   function Stack_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is begin
      return Cursor.Ptr.Stack.Head = null;
   end Stack_Empty;

   function Peek (Cursor : in Parser_Lists.Cursor) return Stack_Item
   is begin
      return Cursor.Ptr.Stack.Head.Item;
   end Peek;

   function Pop (Cursor : in Parser_Lists.Cursor) return Stack_Item
   is
      Result    : constant Stack_Item        := Cursor.Ptr.Stack.Head.Item;
      Temp_Free : constant Stack_Node_Access := Cursor.List.Stack_Free;
   begin
      Cursor.List.Stack_Free := Cursor.Ptr.Stack.Head;
      Cursor.Ptr.Stack.Head  := Cursor.Ptr.Stack.Head.Next;

      Cursor.List.Stack_Free.all :=
        (Item     =>
           (State => Unknown_State,
            Token => null), -- Token is free'd after being passed to user action
         Next     => Temp_Free);

      return Result;
   end Pop;

   procedure Push (Cursor : in Parser_Lists.Cursor; Item : in Stack_Item)
   is
      Temp : constant Stack_Node_Access := Cursor.List.Stack_Free;
   begin
      if Temp = null then
         Cursor.Ptr.Stack.Head := new Stack_Node'(Item, Cursor.Ptr.Stack.Head);
      else
         Cursor.List.Stack_Free := Cursor.List.Stack_Free.Next;
         Temp.all               := (Item, Cursor.Ptr.Stack.Head);
         Cursor.Ptr.Stack.Head  := Temp;
      end if;
   end Push;

   procedure Put_Top_10 (Cursor : in Parser_Lists.Cursor)
   is
      use Ada.Text_IO;
      use type Token.Handle;
      Stack_I : Stack_Node_Access := Cursor.Ptr.Stack.Head;
   begin
      Put (Integer'Image (Cursor.Ptr.Label) & " stack: ");
      for I in 1 .. 10 loop
         exit when Stack_I = null;
         Ada.Text_IO.Put
           (State_Index'Image (Stack_I.Item.State) & " : " &
              (if Stack_I.Item.Token = null then ""
               else Token.Token_Image (Token.ID (Stack_I.Item.Token.all))) &
              ", ");
         Stack_I := Stack_I.Next;
      end loop;
      New_Line;
   end Put_Top_10;

   procedure Append (Cursor : in Parser_Lists.Cursor; Item : in Action_Token)
   is begin
      null; -- FIXME:
   end Append;

   function Pop (Cursor : in Parser_Lists.Cursor) return Action_Token
   is
      pragma Unreferenced (Cursor);
   begin
      return (null, 0, Token_List.Null_List); -- FIXME:
   end Pop;

   function Action_Tokens_Empty (Cursor : in Parser_Lists.Cursor) return Boolean
   is
      pragma Unreferenced (Cursor);
   begin
      return True; -- FIXME:
   end Action_Tokens_Empty;

   function Deep_Copy (Stack : in Parser_Lists.Stack; Stack_Free : in out Stack_Node_Access) return Parser_Lists.Stack
   is
      --  Create a copy in Copy, in reverse order. Then move to
      --  Result.Head, in correct order.
      I      : Stack_Node_Access := Stack.Head;
      Copy   : Stack_Node_Access;
      Temp   : Stack_Node_Access;
      Result : Parser_Lists.Stack;
   begin
      loop
         exit when I = null;
         if Stack_Free = null then
            Copy := new Stack_Node'(I.Item, Copy);
         else
            Temp       := Copy;
            Copy       := Stack_Free;
            Stack_Free := Stack_Free.Next;
            Copy.all   := (I.Item, Temp);
         end if;
         I := I.Next;
      end loop;

      I := Copy;

      loop
         exit when I = null;
         Temp        := I.Next;
         I.Next      := Result.Head;
         Result.Head := I;
         I           := Temp;
      end loop;

      return Result;
   end Deep_Copy;

   procedure Prepend_Copy (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class)
   is
      Temp : constant Parser_State_Access := List.Parser_Free;
   begin
      Parser_Label := Parser_Label + 1;
      List.Count   := List.Count + 1;

      if Temp = null then
         List.Head := new Parser_State'
           (Parser_Label,
            Cursor.Ptr.Verb,
            Deep_Copy (Cursor.Ptr.Stack, List.Stack_Free),
            List.Head);
      else
         List.Parser_Free := List.Parser_Free.Next;
         Temp.all  :=
           (Parser_Label,
            Cursor.Ptr.Verb,
            Deep_Copy (Cursor.Ptr.Stack, List.Stack_Free),
            List.Head);
         List.Head := Temp;
      end if;
   end Prepend_Copy;

   procedure Free (List : in out Parser_Lists.List; Cursor : in Parser_Lists.Cursor'Class)
   is begin
      null; -- FIXME:
   end Free;

   ----------
   --  stuff for iterators

   function Constant_Reference
     (Container : aliased in List'Class;
      Position  : in Iterator_Cursor)
     return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Position);
   end Constant_Reference;

   type List_Access_Constant is access constant List;

   type Iterator is new Iterator_Interfaces.Forward_Iterator with record
      Container : List_Access_Constant;
   end record;

   overriding function First (Object : Iterator) return Iterator_Cursor;
   overriding function Next
     (Object   : Iterator;
      Position : Iterator_Cursor)
     return Iterator_Cursor;

   overriding function First (Object : Iterator) return Iterator_Cursor
   is begin
      return Iterator_Cursor (Object.Container.Head);
   end First;

   overriding function Next
     (Object   : Iterator;
      Position : Iterator_Cursor)
     return Iterator_Cursor
   is
      pragma Unreferenced (Object);
   begin
      if Position = null then
         return null;
      else
         return Iterator_Cursor (Position.Next);
      end if;
   end Next;

   function Has_Element (Cursor : in Iterator_Cursor) return Boolean
   is begin
      return Cursor /= null;
   end Has_Element;

   function Verb (Cursor : in Iterator_Cursor) return Parse_Action_Verbs
   is begin
      return Cursor.Verb;
   end Verb;

   function Iterate (Container : aliased List) return Iterator_Interfaces.Forward_Iterator'Class
   is begin
      return Iterator'(Container => Container'Access);
   end Iterate;

   ----------
   --  For unit tests

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

end Parser_Lists;
