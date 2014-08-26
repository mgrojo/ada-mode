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

   type Stack_Node;
   type Stack_Node_Access is access Stack_Node;
   type Stack_Node is record
      Item : Stack_Item;
      Next : Stack_Node_Access;
   end record;

   type Stack is record
      Head : Stack_Node_Access;
      Free : Stack_Node_Access;
      --  Popped nodes go on the free list, pushed nodes come from
      --  the free list.
   end record;

   --  FIXME: procedure Free is new Ada.Unchecked_Deallocation (Stack_Node, Stack_Node_Access);

   type Parser_State is record
      Verb  : Parse_Action_Verbs; -- last action performed
      Stack : Parser_Lists.Stack;
      --  Pending_Actions : Action_Token_List; -- FIXME: accumulated actions while parallel parsing
      Next  : Parser_State_Access;
   end record;

   --  FIXME: procedure Free is new Ada.Unchecked_Deallocation (Parser_State, Parser_State_Access);

   function Initialize return List
   is begin
      return
        (Head              => new Parser_State'
           (Verb           => Parse_Action_Verbs'First,
            Stack          =>
              (Head        => new Stack_Node'
                 (Item     =>
                    (State => State_Index'First,
                     Token => null),
                  Next     => null),
               Free        => null),
            Next           => null),
         Free              => null,
         Count             => 1);

   end Initialize;

   function Count (List : in Parser_Lists.List) return Integer
   is begin
      return List.Count;
   end Count;

   function First (List : in Parser_Lists.List'Class) return Iterator
   is begin
      return (Ptr => List.Head);
   end First;

   procedure Next (Iter : in out Iterator)
   is begin
      if Iter.Ptr /= null then
         Iter.Ptr := Iter.Ptr.Next;
      end if;
   end Next;

   function Is_Done (Iter : in Iterator) return Boolean
   is begin
      return Iter.Ptr = null;
   end Is_Done;

   procedure Set_Verb (Iter : in Iterator; Verb : in Parse_Action_Verbs)
   is begin
      Iter.Ptr.Verb := Verb;
   end Set_Verb;

   function Verb (Iter : in Iterator) return Parse_Action_Verbs
   is begin
      return Iter.Ptr.Verb;
   end Verb;

   function Peek (Iter : in Iterator) return Stack_Item
   is begin
      return Iter.Ptr.Stack.Head.Item;
   end Peek;

   function Pop (Iter : in Iterator) return Stack_Item
   is
      Result : constant Stack_Item := Iter.Ptr.Stack.Head.Item;
   begin
      Iter.Ptr.Stack.Free      := Iter.Ptr.Stack.Head;
      Iter.Ptr.Stack.Head      := Iter.Ptr.Stack.Head.Next;
      Iter.Ptr.Stack.Free.Next := Iter.Ptr.Stack.Free;
      Iter.Ptr.Stack.Free.Item :=
        (State => Unknown_State,
         Token => null); -- Token is free'd after being passed to user action
      return Result;
   end Pop;

   procedure Push (Iter : in Iterator; Item : in Stack_Item)
   is
      Temp : constant Stack_Node_Access := Iter.Ptr.Stack.Free;
   begin
      if Temp = null then
         Iter.Ptr.Stack.Head := new Stack_Node'(Item, Iter.Ptr.Stack.Head);
      else
         Iter.Ptr.Stack.Free := Iter.Ptr.Stack.Free.Next;
         Temp.all            := (Item, Iter.Ptr.Stack.Head);
         Iter.Ptr.Stack.Head := Temp;
      end if;
   end Push;

   procedure Put_Top_10 (Iter : in Iterator)
   is
      use type Token.Handle;
      Stack_I : Stack_Node_Access := Iter.Ptr.Stack.Head;
   begin
      for I in 1 .. 10 loop
         exit when Stack_I = null;
         Ada.Text_IO.Put_Line
           (State_Index'Image (Stack_I.Item.State) & " : " &
              (if Stack_I.Item.Token = null then ""
               else Token.Token_Image (Token.ID (Stack_I.Item.Token.all))));
         Stack_I := Stack_I.Next;
      end loop;
   end Put_Top_10;

   procedure Append (Iter : in Iterator; Item : in Action_Token)
   is begin
      null; -- FIXME:
   end Append;

   function Pop (Iter : in Iterator) return Action_Token
   is
      pragma Unreferenced (Iter);
   begin
      return (null, 0, Token_List.Null_List); -- FIXME:
   end Pop;

   function Action_Tokens_Empty (Iter : in Iterator) return Boolean
   is
      pragma Unreferenced (Iter);
   begin
      return True; -- FIXME:
   end Action_Tokens_Empty;

   function Deep_Copy (Stack : in Parser_Lists.Stack) return Parser_Lists.Stack
   is
      --  Create a copy in Result.Free, in reverse order. Then move to
      --  Result.Head, in correct order.
      I      : Stack_Node_Access := Stack.Head;
      Temp   : Stack_Node_Access;
      Result : Parser_Lists.Stack;
   begin
      loop
         exit when I = null;
         Result.Free := new Stack_Node'(I.Item, Result.Free);
         I           := I.Next;
      end loop;

      I := Result.Free;

      loop
         exit when I = null;
         Temp        := I.Next;
         I.Next      := Result.Head;
         Result.Head := I;
         I           := Temp;
      end loop;

      Result.Free := null;
      return Result;
   end Deep_Copy;

   procedure Prepend_Copy (List : in out Parser_Lists.List; Iter : in Iterator'Class)
   is
      Temp : constant Parser_State_Access := List.Free;
   begin
      if Temp = null then
         List.Head := new Parser_State'
           (Iter.Ptr.Verb,
            Deep_Copy (Iter.Ptr.Stack),
            List.Head);
      else
         List.Free := List.Free.Next;
         Temp.all  :=
           (Iter.Ptr.Verb,
            Deep_Copy (Iter.Ptr.Stack),
            List.Head);
         List.Head := Temp;
      end if;
   end Prepend_Copy;

   procedure Free (List : in out Parser_Lists.List; Iter : in Iterator'Class)
   is begin
      null; -- FIXME:
   end Free;

end Parser_Lists;
