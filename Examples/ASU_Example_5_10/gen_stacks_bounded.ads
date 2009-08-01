--  Abstract:
--
--  A simple bounded stack
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

pragma License (Modified_GPL);

generic
   type Item_Type is private;
   Null_Item : in Item_Type;
package Gen_Stacks_Bounded is
   pragma Pure;

   Container_Empty : exception;
   Container_Full : exception;

   type Stack_Type (Size : Positive) is private;
   --  Size is maximum number of items on the stack.

   ------------
   --  Operations on Stack_Type (alphabetical order)

   procedure Clear (Stack : in out Stack_Type);
   --  Empty Stack of all items.
   pragma Inline (Clear);

   function Depth (Stack : in Stack_Type) return Natural;
   --  Returns current number of items on Stack
   pragma Inline (Depth);

   function Is_Empty (Stack : in Stack_Type) return Boolean;
   --  Returns true if no items are in Stack.
   pragma Inline (Is_Empty);

   function Is_Full (Stack : in Stack_Type) return Boolean;
   --  Returns true if Stack is full.
   pragma Inline (Is_Full);

   function Peek (Stack : in Stack_Type; Index : in Positive) return Item_Type;
   --  Return the Index'th item from the top of Stack; the Item is
   --  _not_ removed. Top item has index 1.
   --
   --  Raises Constraint_Error if Index > Depth.
   pragma Inline (Peek);

   procedure Pop (Stack : in out Stack_Type);
   --  Remove top item from Stack.
   --
   --  Raises Container_Empty if Is_Empty.
   pragma Inline (Pop);

   procedure Push (Stack : in out Stack_Type; Item : in Item_Type);
   --  Add Item to the top of Stack.
   --
   --  Raises Container_Full if Is_Full.
   pragma Inline (Push);

   function Top (Stack : in Stack_Type) return Item_Type;
   --  Return the item at the top of Stack; the Item is _not_ removed.
   --
   --  Raises Container_Empty if Is_Empty.
   pragma Inline (Top);

private

   type Item_Array_Type is array (Positive range <>) of Item_Type;

   type Stack_Type (Size : Positive) is record

      Top       : Natural                     := 0;
      Max_Depth : Natural                     := 0;
      Items     : Item_Array_Type (1 .. Size) := (others => Null_Item);
   end record;

end Gen_Stacks_Bounded;
