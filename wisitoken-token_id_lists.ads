--  Abstract :
--
--  A list of Token_IDs.
--
--  List is _not_ Controlled, so assignment of the List type does a
--  shallow copy of the root pointers. The WisiToken grammar generator
--  code takes advantage of this.
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

with Ada.Containers;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
package WisiToken.Token_ID_Lists is

   type List is tagged private;
   --  List is _not_ Controlled; assignment does a shallow copy of
   --  the root list pointers. WisiToken.LR.LR1_Items takes advantage of
   --  this.

   Empty_List : constant List;

   function Length (Container : in List) return Ada.Containers.Count_Type;

   procedure Append (Container : in out List; Element : in Token_ID);

   procedure Prepend (Container : in out List; Element : in Token_ID);

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : in Cursor) return Boolean;

   function First (Container : in List) return Cursor;
   function Last (Container : in List) return Cursor;

   procedure Next (Position : in out Cursor);
   function Next (Position : in Cursor) return Cursor;

   procedure Prev (Position : in out Cursor);

   function Element (Position : in Cursor) return Token_ID;

   procedure Delete (Container : in out List; Position : in out Cursor);

private
   type Node_Type;

   type Node_Access is access Node_Type;

   type Node_Type is record
      Element : aliased Token_ID;
      Prev    : Node_Access;
      Next    : Node_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   type List is new Ada.Finalization.Controlled with record
      Head : Node_Access := null;
      Tail : Node_Access := null;
   end record;

   type Cursor is record
      Ptr : Node_Access;
   end record;

   Empty_List : constant List := (Ada.Finalization.Controlled with null, null);

   No_Element : constant Cursor := (Ptr => null);

end WisiToken.Token_ID_Lists;
