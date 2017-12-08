--  Abstract:
--
--  An unbounded minimum Fibonacci heap of definite non-limited elements.
--
--  References:
--
--  [1] Introduction to Algorithms, Third Edition. Thomas H. Cormen,
--  Charles E. Leiserson, Ronald L. Rivest, Clifford Stein. Chapter 19.
--
--  Copyright (C) 2017 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Finalization;
generic
   type Element_Type is private;
   type Key_Type is private;
   with function Key (Item : in Element_Type) return Key_Type;
   with procedure Set_Key (Item : in out Element_Type; Key : in Key_Type);
   pragma Unreferenced (Set_Key); -- needed for Decrease_Key
   with function "<" (Left, Right : in Key_Type) return Boolean is <>;
package SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci is

   type Heap_Type is new Ada.Finalization.Limited_Controlled with private;
   --  Limited so we don't have to implement Adjust

   Empty_Heap : constant Heap_Type;

   overriding
   procedure Initialize (Object : in out Heap_Type);

   overriding
   procedure Finalize (Object : in out Heap_Type);

   procedure Clear (Heap : in out Heap_Type);
   --  Empty Heap (may not free memory; use Finalize for that).

   function Count (Heap : in Heap_Type) return Base_Peek_Type;
   --  Return count of elements in Heap.

   function Remove (Heap : in out Heap_Type) return Element_Type;
   --  Remove minimum element in Heap, return it.

   function Min_Key (Heap : in out Heap_Type) return Key_Type;
   --  Return a copy of the minimum key value.

   function Get (Heap : in out Heap_Type) return Element_Type renames Remove;

   procedure Drop (Heap : in out Heap_Type);
   --  Remove minimum element in Heap, discard it.

   procedure Add (Heap : in out Heap_Type; Item : in Element_Type);
   --  Add Item to Heap.

   procedure Insert (Heap : in out Heap_Type; Item : in Element_Type) renames Add;

   --  procedure Increase_Key (Heap : in out Heap_Type; index : in index_type; Item : in Element_Type);
   --  IMPROVEME: implement. need Index (heap, Key), or Add return index.
private

   type Node;
   type Node_Access is access Node;

   type Node is record
      Element : Element_Type;
      Parent  : Node_Access;
      Child   : Node_Access;
      Left    : Node_Access;
      Right   : Node_Access;
      Degree  : Natural;
      Mark    : Boolean;
   end record;

   type Heap_Type is new Ada.Finalization.Limited_Controlled with record
      Min   : Node_Access;
      Count : Base_Peek_Type;
   end record;

   Empty_Heap : constant Heap_Type := (Ada.Finalization.Limited_Controlled with Min => null, Count => 0);

end SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
