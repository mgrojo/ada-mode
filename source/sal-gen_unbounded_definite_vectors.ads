--  Abstract :
--
--  A simple unbounded vector of definite items, intended to be faster
--  than Ada.Containers.Vectors.
--
--  Prepend is as fast (in amortized time) as Append.
--
--  It provides no checking of cursor tampering; higher level code
--  must ensure that.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Unchecked_Deallocation;
generic
   type Index_Type is range <>;
   type Element_Type is private;
package SAL.Gen_Unbounded_Definite_Vectors is

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
           Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   type Vector is new Ada.Finalization.Controlled with private with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Variable_Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   Empty_Vector : constant Vector;

   overriding procedure Finalize (Container : in out Vector);
   overriding procedure Adjust (Container : in out Vector);

   function Length (Container : in Vector) return Ada.Containers.Count_Type;
   function Capacity (Container : in Vector) return Ada.Containers.Count_Type;

   procedure Clear (Container : in out Vector)
   renames Finalize;

   function First_Index (Container : Vector) return Extended_Index;
   function Last_Index (Container : Vector) return Extended_Index;
   --  No_Index when Container is empty.

   function Element (Container : Vector; Index : Index_Type) return Element_Type;
   --  Index of first element in vector is Index_Type'First.

   procedure Append (Container : in out Vector; New_Item : in Element_Type);
   --  Insert New_Item at end of Container.
   --
   --  Raises Constraint_Error if index of new item would be greater than
   --  Index_Type'Last.

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type);
   --  Insert New_Item at beginning of Container.
   --
   --  Raises Constraint_Error if index of new item would be less than
   --  Index_Type'First.

   procedure Prepend
     (Target       : in out Vector;
      Source       : in     Vector;
      Source_First : in     Index_Type;
      Source_Last  : in     Index_Type);
   --  Copy Source (Source_First .. Source_Last) to Target, before
   --  Target.First_Index.

   function To_Vector (Item : in Element_Type; Count : in Ada.Containers.Count_Type) return Vector;

   function "&" (Left, Right : in Element_Type) return Vector;
   function "&" (Left : in Vector; Right : in Element_Type) return Vector;

   procedure Set_First (Container : in out Vector; First : in Index_Type);
   procedure Set_Last (Container : in out Vector; Last : in Index_Type);
   --  Default First is Index_Type'First.
   --  Elements with First <= index <= Last that have not been set have
   --  Element_Type default value.

   procedure Set_Length (Container : in out Vector; Length : in Ada.Containers.Count_Type);
   --  Set Last so Container.Length returns Length.

   procedure Delete (Container : in out Vector; Index : in Index_Type);
   --  Replace Index element contents with default. If Index =
   --  Container.Last_Index, Container.Last_Index is decremented.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is null record
   with Implicit_Dereference => Element;

   function Constant_Reference (Container : aliased in Vector; Index : in Index_Type) return Constant_Reference_Type
   with Pre => Index >= Container.First_Index or Index <= Container.Last_Index;

   type Variable_Reference_Type (Element : not null access Element_Type) is null record
   with Implicit_Dereference => Element;

   function Variable_Reference (Container : aliased in Vector; Index : in Index_Type) return Variable_Reference_Type
   with Pre => Index >= Container.First_Index or Index <= Container.Last_Index;

   type Cursor is private;

   function Has_Element (Position : Cursor) return Boolean;

   package Vector_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : aliased in Vector) return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   function Constant_Reference (Container : aliased in Vector; Position : in Cursor) return Constant_Reference_Type
   with Pre => Has_Element (Position);

   function Variable_Reference (Container : aliased in Vector; Position  : in Cursor) return Variable_Reference_Type
   with Pre => Has_Element (Position);
private

   type Array_Type is array (SAL.Peek_Type range <>) of aliased Element_Type;
   type Array_Access is access Array_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Array_Type, Array_Access);

   type Vector is new Ada.Finalization.Controlled with
   record
      Elements : Array_Access;
      First    : Extended_Index := No_Index;
      Last     : Extended_Index := No_Index;
   end record;

   type Vector_Access is access all Vector;
   for Vector_Access'Storage_Size use 0;

   type Cursor is record
      Container : Vector_Access;
      Index     : Base_Peek_Type;
   end record;

   type Iterator is new Vector_Iterator_Interfaces.Reversible_Iterator with
   record
      Container : Vector_Access;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   Empty_Vector : constant Vector := (Ada.Finalization.Controlled with others => <>);

   ----------
   --  Visible for child package

   function To_Peek_Type (Item : in Index_Type) return Peek_Type'Base with Inline;

end SAL.Gen_Unbounded_Definite_Vectors;
