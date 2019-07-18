--  Abstract :
--
--  A simple bounded sorted vector of definite items.
--
--  Copyright (C) 2018, 2019 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Iterator_Interfaces;
generic
   type Element_Type is private;
   with function Element_Compare (Left, Right : in Element_Type) return Compare_Result;
   Capacity : in Ada.Containers.Count_Type;
package SAL.Gen_Bounded_Definite_Vectors_Sorted is

   type Vector is tagged private with
      Constant_Indexing => Constant_Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   Empty_Vector : constant Vector;

   function Length (Container : in Vector) return Ada.Containers.Count_Type;

   function Is_Full (Container : in Vector) return Boolean;

   procedure Clear (Container : in out Vector);

   procedure Insert
     (Container       : in out Vector;
      New_Item        : in     Element_Type;
      Ignore_If_Equal : in     Boolean := False);
   --  Insert New_Item in sorted position. Items are sorted in increasing
   --  order according to Element_Compare. New_Item is inserted after
   --  Equal items, unless Ignore_If_Equal is true, in which case
   --  New_Item is not inserted.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is null record
   with Implicit_Dereference => Element;

   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   package Vector_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Vector) return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference (Container : aliased Vector; Position : in Cursor) return Constant_Reference_Type
   with Inline;

   function First_Index (Container : aliased Vector) return Peek_Type is (Peek_Type'First);
   function Last_Index (Container : aliased Vector) return Base_Peek_Type
   with Inline;
   function Constant_Reference (Container : aliased Vector; Index : in Peek_Type) return Constant_Reference_Type
   with Inline;
private

   type Array_Type is array (Peek_Type range 1 .. Peek_Type (Capacity)) of aliased Element_Type;

   No_Index : constant Base_Peek_Type := 0;

   type Vector is tagged
   record
      Elements : Array_Type     := (others => <>);
      Last     : Base_Peek_Type := No_Index;
   end record;

   type Vector_Access is access all Vector;
   for Vector_Access'Storage_Size use 0;

   type Cursor is record
      Container : Vector_Access;
      Index     : Base_Peek_Type := No_Index;
   end record;

   type Iterator is new Vector_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Vector_Access;
      Index     : Base_Peek_Type;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   Empty_Vector : constant Vector := (others => <>);

end SAL.Gen_Bounded_Definite_Vectors_Sorted;
