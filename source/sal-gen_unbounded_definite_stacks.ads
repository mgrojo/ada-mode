--  Abstract:
--
--  Stack implementation using Ada.Containers.Vectors.
--
--  Copyright (C) 1998-2000, 2002-2003, 2009, 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
generic
   type Element_Type is private;
package SAL.Gen_Unbounded_Definite_Stacks is

   type Stack_Type is new Ada.Finalization.Controlled with private;

   Empty_Stack : constant Stack_Type;

   overriding procedure Finalize (Stack : in out Stack_Type);
   overriding procedure Adjust (Stack : in out Stack_Type);

   overriding function "=" (Left, Right : in Stack_Type) return Boolean;

   procedure Clear (Stack : in out Stack_Type);
   --  Empty Stack of all items.

   function Depth (Stack : in Stack_Type) return Base_Peek_Type;
   --  Returns current count of items in the Stack

   function Is_Empty (Stack : in Stack_Type) return Boolean;
   --  Returns true iff no items are in Stack.

   function Peek
     (Stack : in Stack_Type;
      Index : in Peek_Type := 1)
     return Element_Type;
   --  Return the Index'th item from the top of Stack; the Item is _not_ removed.
   --  Top item has index 1.
   --
   --  Raises Constraint_Error if Index > Depth.

   procedure Pop (Stack : in out Stack_Type);
   --  Remove Item from the top of Stack, discard it.
   --
   --  Raises Container_Empty if Is_Empty.

   function Pop (Stack : in out Stack_Type) return Element_Type;
   --  Remove Item from the top of Stack, and return it.
   --
   --  Raises Container_Empty if Is_Empty.

   procedure Push (Stack : in out Stack_Type; Item : in Element_Type);
   --  Add Item to the top of Stack.
   --
   --  May raise Container_Full.

   function Top (Stack : in Stack_Type) return Element_Type;
   --  Return the item at the top of Stack; the Item is _not_ removed.
   --  Same as Peek (Stack, 1).
   --
   --  Raises Container_Empty if Is_Empty.

   procedure Set_Depth
     (Stack : in out Stack_Type;
      Depth : in     Peek_Type);
   --  Empty Stack, set its Depth to Depth. Must be followed by Set
   --  for each element.
   --
   --  Useful when creating a stack from pre-existing data.

   procedure Set
     (Stack   : in out Stack_Type;
      Index   : in     Peek_Type;
      Depth   : in     Peek_Type;
      Element : in     Element_Type);
   --  Set a Stack element. Index is the same as Peek Index; Depth is
   --  used to compute the index in the underlying array.
   --
   --  Stack must have been initialized by Set_Depth.
   --
   --  Useful when creating a stack from pre-existing data.

private

   type Element_Array is array (Peek_Type range <>) of Element_Type;
   type Element_Array_Access is access Element_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Element_Array, Element_Array_Access);

   type Stack_Type is new Ada.Finalization.Controlled with record
      Top  : Base_Peek_Type := Invalid_Peek_Index; -- empty
      Data : Element_Array_Access;

      --  Top of stack is at Data (Top).
      --  Data (1 .. Last_Index) has been set at some point.
   end record;

   Empty_Stack : constant Stack_Type := (Ada.Finalization.Controlled with Invalid_Peek_Index, null);

end SAL.Gen_Unbounded_Definite_Stacks;
