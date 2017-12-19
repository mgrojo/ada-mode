--  Abstract :
--
--  A simple bounded vector of definite items, intended to be faster
--  than Ada.Containers.Bounded_Definite_Vectors.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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
   type Index_Type is range <>;
   type Element_Type is private;
   Capacity : in SAL.Peek_Type;
package SAL.Gen_Bounded_Definite_Vectors is

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
           Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   type Vector is tagged private with
      Constant_Indexing => Constant_Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   Empty_Vector : constant Vector;

   function Length (Container : in Vector) return SAL.Base_Peek_Type;

   procedure Clear (Container : in out Vector);

   function First_Index (Container : Vector) return Index_Type is (Index_Type'First);

   function Last_Index (Container : Vector) return Extended_Index;
   --  No_Index when Container is empty.

   function Element (Container : Vector; Index : Index_Type) return Element_Type;
   --  Index of first element in vector is Index_Type'First.

   procedure Append (Container : in out Vector; New_Item : in Element_Type);

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type);
   --  Insert New_Item at beginning of Container; current elements slide right.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference (Container : aliased Vector; Index : in Index_Type) return Constant_Reference_Type;

   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   package Vector_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Vector) return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference (Container : aliased Vector; Position : in Cursor) return Constant_Reference_Type;

private

   type Array_Type is array (SAL.Peek_Type range 1 .. Capacity) of aliased Element_Type;

   type Vector is tagged
   record
      Elements : Array_Type := (others => <>);
      Last     : Extended_Index := No_Index;
   end record;

   type Vector_Access is access all Vector;
   for Vector_Access'Storage_Size use 0;

   type Cursor is record
      Container : Vector_Access;
      Index     : Index_Type := Index_Type'First;
   end record;

   type Iterator is new Vector_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Vector_Access;
      Index     : Index_Type'Base;
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

end SAL.Gen_Bounded_Definite_Vectors;
