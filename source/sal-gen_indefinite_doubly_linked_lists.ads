--  Abstract :
--
--  A generic doubly linked list with indefinite elements.
--
--  Copyright (C) 2018 - 2022 Free Software Foundation, Inc.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type (<>) is private;
package SAL.Gen_Indefinite_Doubly_Linked_Lists is

   type List is new Ada.Finalization.Controlled with private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Variable_Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type;

   Empty_List : constant List;

   overriding procedure Adjust (Container : in out List);
   --  Deep copy.

   overriding procedure Finalize (Container : in out List);
   --  Free all items in List.

   function Length (Container : in List) return Base_Peek_Type;

   procedure Append (Container : in out List; Element : in Element_Type);

   procedure Prepend (Container : in out List; Element : in Element_Type);

   function To_List (Element : in Element_Type) return List;

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : in Cursor) return Boolean;

   function First (Container : in List) return Cursor;
   function Last (Container : in List) return Cursor;

   procedure Next (Position : in out Cursor);

   function Next (Position : in Cursor) return Cursor;
   function Previous (Position : in Cursor) return Cursor;

   function Element (Position : in Cursor) return Element_Type
   with Pre => Has_Element (Position);

   function Append (Container : in out List; Element : in Element_Type) return Cursor;

   procedure Delete (Container : in out List; Position : in out Cursor)
   with Pre => Has_Element (Position);

   function Unchecked_Ref (Position : in Cursor) return access Element_Type
   with Pre => Has_Element (Position);
   --  For use in building higher-level containers.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private with
     Implicit_Dereference => Element;

   function Constant_Ref (Position : in Cursor) return Constant_Reference_Type
   with Inline, Pre => Has_Element (Position);

   function Constant_Reference (Container : in List; Position : in Cursor) return Constant_Reference_Type
   with Inline, Pre => Has_Element (Position);

   type Variable_Reference_Type (Element : not null access Element_Type) is private with
     Implicit_Dereference => Element;

   function Variable_Reference (Container : in List; Position : in Cursor) return Variable_Reference_Type
   with Inline, Pre => Has_Element (Position);

   function Variable_Ref (Position : in Cursor) return Variable_Reference_Type
   with Inline, Pre => Has_Element (Position);

   package Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : aliased in List) return Iterator_Interfaces.Reversible_Iterator'Class;

private
   type Node_Type;
   type Node_Access is access Node_Type;
   type Element_Access is access Element_Type;


   type Node_Type is record
      Element : Element_Access;
      Prev    : Node_Access;
      Next    : Node_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   type List is new Ada.Finalization.Controlled with record
      Head  : Node_Access        := null;
      Tail  : Node_Access        := null;
      Count : SAL.Base_Peek_Type := 0;
   end record;

   type Cursor is record
      Ptr : Node_Access;
   end record;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   type Variable_Reference_Type (Element : not null access Element_Type) is
   record
      Dummy : Integer := raise Program_Error with "uninitialized reference";
   end record;

   Empty_List : aliased constant List := (Ada.Finalization.Controlled with null, null, 0);

   No_Element : constant Cursor := (Ptr => null);

   type Iterator (Container : not null access constant List) is new Iterator_Interfaces.Reversible_Iterator with
   null record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end SAL.Gen_Indefinite_Doubly_Linked_Lists;
