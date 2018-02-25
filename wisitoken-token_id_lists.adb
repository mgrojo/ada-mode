--  Abstract :
--
--  see spec
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.Token_ID_Lists is

   ---------
   --  Public operations, declaration order.

   function Length (Container : in List) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Node   : Node_Access := Container.Head;
      Result : Count_Type  := 0;
   begin
      loop
         exit when Node = null;
         Result := Result + 1;
         Node   := Node.Next;
      end loop;

      return Result;
   end Length;

   procedure Append (Container : in out List; Element : in Token_ID)
   is
      use all type Ada.Containers.Count_Type;
      New_Node : constant Node_Access := new Node_Type'
        (Element => Element,
         Prev    => Container.Tail,
         Next    => null);
   begin
      if Container.Tail = null then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Tail.Next := New_Node;
         Container.Tail      := New_Node;
      end if;
   end Append;

   procedure Prepend (Container : in out List; Element : in Token_ID)
   is
      use all type Ada.Containers.Count_Type;
      New_Node : constant Node_Access := new Node_Type'
        (Element => Element,
         Prev    => null,
         Next    => Container.Head);
   begin
      if Container.Tail = null then
         Container.Head := New_Node;
         Container.Tail := New_Node;
      else
         Container.Head.Prev := New_Node;
         Container.Head      := New_Node;
      end if;
   end Prepend;

   function Has_Element (Position : in Cursor) return Boolean
   is begin
      return Position.Ptr /= null;
   end Has_Element;

   function First (Container : in List) return Cursor
   is begin
      if Container.Head = null then
         return No_Element;
      else
         return (Ptr => Container.Head);
      end if;
   end First;

   function Last (Container : in List) return Cursor
   is begin
      if Container.Head = null then
         return No_Element;
      else
         return (Ptr => Container.Tail);
      end if;
   end Last;

   procedure Next (Position : in out Cursor)
   is begin
      if Position.Ptr /= null then
         if Position.Ptr.Next = null then
            Position := No_Element;
         else
            Position.Ptr := Position.Ptr.Next;
         end if;
      end if;
   end Next;

   function Next (Position : in Cursor) return Cursor
   is begin
      if Position.Ptr = null then
         return Position;
      else
         if Position.Ptr.Next = null then
            return No_Element;
         else
            return (Ptr => Position.Ptr.Next);
         end if;
      end if;
   end Next;

   procedure Prev (Position : in out Cursor)
   is begin
      if Position.Ptr /= null then
         if Position.Ptr.Prev = null then
            Position := No_Element;
         else
            Position.Ptr := Position.Ptr.Prev;
         end if;
      end if;
   end Prev;

   function Element (Position : in Cursor) return Token_ID
   is begin
      return Position.Ptr.Element;
   end Element;

   procedure Delete (Container : in out List; Position : in out Cursor)
   is
      use all type Ada.Containers.Count_Type;
      Node : Node_Access renames Position.Ptr;
   begin
      if Node.Next = null then
         Container.Tail := Node.Prev;
      else
         Node.Next.Prev := Node.Prev;
      end if;
      if Node.Prev = null then
         Container.Head := Node.Next;
      else
         Node.Prev.Next := Node.Next;
      end if;
      Free (Node);
      Position := No_Element;
   end Delete;

end WisiToken.Token_ID_Lists;
