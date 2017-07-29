--  Abstract :
--
--  see spec
--
--
--  Copyright (C) 2009, 2014, 2015, 2017 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details. You should have received a copy of the
--  GNU General Public License distributed with the WisiToken package;
--  see file GPL.txt. If not, write to the Free Software Foundation,
--  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

with Ada.Unchecked_Deallocation;
package body WisiToken.Token is

   package body List is

      procedure Free is new Ada.Unchecked_Deallocation (List_Node, List_Node_Ptr);

      function Is_Empty (Item : in Instance) return Boolean
      is begin
         return Item.Head = null;
      end Is_Empty;

      function Length (Item : in Instance) return Ada.Containers.Count_Type
      is
         use Ada.Containers;
         Node   : List_Node_Ptr := Item.Head;
         Result : Count_Type    := 0;
      begin
         loop
            exit when Node = null;
            Result := Result + 1;
            Node   := Node.Next;
         end loop;

         return Result;
      end Length;

      function Only (Subject : in Token_ID) return Instance
      is
         New_Node : constant List_Node_Ptr := new List_Node'
           (ID   => Subject,
            Next => null);
      begin
         return
           (Head => New_Node,
            Tail => New_Node);
      end Only;

      function Deep_Copy (Item : in Instance) return Instance
      is
         Result : Instance;
         I : List_Iterator := Item.First;
      begin
         loop
            exit when Is_Null (I);
            Result := Result & ID (I);
            Next (I);
         end loop;
         return Result;
      end Deep_Copy;

      function "&" (Left : in Token_ID; Right : in Token_ID) return Instance
      is
         Right_Node : constant List_Node_Ptr := new List_Node'
           (ID   => Right,
            Next => null);
      begin
         return
           (Head    => new List_Node'
              (ID   => Left,
               Next => Right_Node),
            Tail    => Right_Node);
      end "&";

      function "&" (Left : in Instance; Right : in Token_ID) return Instance
      is begin
         if Left.Head = null then
            return Only (Right);
         else
            Left.Tail.Next := new List_Node'(Right, null);
            return (Left.Head, Left.Tail.Next);
         end if;
      end "&";

      procedure Prepend (List : in out Instance; Item : in Token_ID)
      is
         New_Node : constant List_Node_Ptr := new List_Node'(Item, List.Head);
      begin
         if List.Tail = null then
            List.Tail := New_Node;
         end if;
         List.Head := New_Node;
      end Prepend;

      procedure Append (List  : in out Instance; Item : in Token_ID)
      is
         New_Node : constant List_Node_Ptr := new List_Node'(Item, null);
      begin
         if List.Tail = null then
            List.Head := New_Node;
         else
            List.Tail.Next := New_Node;
         end if;

         List.Tail := New_Node;
      end Append;

      procedure Clean (List : in out Instance)
      is
         Node : List_Node_Ptr := List.Head;
         Next : List_Node_Ptr;
      begin
         --  Deallocate all the nodes in the list, along with all their tokens
         while Node /= null loop
            Next := Node.Next;
            Free (Node);
            Node := Next;
         end loop;

         List.Head := null;
         List.Tail := null;
      end Clean;

      function First (List : in Instance) return List_Iterator is
      begin
         return List_Iterator (List.Head);
      end First;

      function Pop (List : in out Instance) return Token_ID
      is
         Result : constant Token_ID := List.Head.ID;
         Temp   : List_Node_Ptr     := List.Head;
      begin
         List.Head := List.Head.Next;
         if List.Tail = Temp then
            List.Tail := null;
         end if;
         Free (Temp);
         return Result;
      end Pop;

      function Peek (List : in out Instance; I : in Integer) return Token_ID
      is
         Temp : List_Node_Ptr := List.Head;
      begin
         for J in 1 .. I - 1 loop
            Temp := Temp.Next;
         end loop;
         return Temp.ID;
      end Peek;

      procedure Next (Iterator : in out List_Iterator) is
      begin
         if Iterator /= null then
            Iterator := List_Iterator (Iterator.Next);
         end if;
      end Next;

      function Next (Iterator : in List_Iterator) return List_Iterator
      is begin
         return List_Iterator (Iterator.Next);
      end Next;

      function Is_Done (Iterator : in List_Iterator) return Boolean
      is begin
         return Iterator = null;
      end Is_Done;

      function Current (Iterator : in List_Iterator) return Token_ID
      is begin
         return Iterator.ID;
      end Current;

      procedure Put (Trace : in out WisiToken.Trace'Class; Item : in Instance)
      is
         I : List_Iterator := Item.First;
      begin
         loop
            exit when I = Null_Iterator;
            Trace.Put (Image (Trace.Descriptor.all, I.ID));
            Next (I);
            if I /= Null_Iterator then
               Trace.Put (", ");
            end if;
         end loop;
      end Put;

   end List;

end WisiToken.Token;
