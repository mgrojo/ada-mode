--  Abstract:
--
--  An unbounded queue of definite non-limited elements.
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

with Ada.Containers.Doubly_Linked_Lists;
with SAL.Gen_Queue_Interfaces;
generic
   type Element_Type is private;
   with package Queue_Interfaces is new SAL.Gen_Queue_Interfaces (Element_Type);
package SAL.Gen_Unbounded_Definite_Queues is

   type Queue_Type is new Queue_Interfaces.Queue_Type with private;

   Empty_Queue : constant Queue_Type;

   overriding procedure Clear (Queue : in out Queue_Type);
   overriding function Count (Queue : in Queue_Type) return Base_Peek_Type;
   overriding function Length (Queue : in Queue_Type) return Base_Peek_Type renames Count;
   overriding function Is_Empty (Queue : in Queue_Type) return Boolean;
   overriding function Is_Full (Queue : in Queue_Type) return Boolean is (False);
   overriding function Remove (Queue : in out Queue_Type) return Element_Type;
   overriding function Get (Queue : in out Queue_Type) return Element_Type renames Remove;
   overriding procedure Drop (Queue : in out Queue_Type);
   overriding function Peek (Queue : in Queue_Type; N : Peek_Type := 1) return Element_Type;
   overriding procedure Add (Queue : in out Queue_Type; Item : in Element_Type);
   overriding procedure Put (Queue : in out Queue_Type; Item : in Element_Type) renames Add;
   overriding procedure Add_To_Head (Queue : in out Queue_Type; Item : in Element_Type);
   overriding procedure Add
     (Queue   : in out Queue_Type;
      Element : in     Element_Type;
      Order   : in     Queue_Interfaces.Order_Type);

private

   package Element_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Element_Type);

   type Queue_Type is new Queue_Interfaces.Queue_Type with record
      Data : Element_Lists.List;
      --  Add at Tail = Last, remove at Head = First.
   end record;

   Empty_Queue : constant Queue_Type := (Data => Element_Lists.Empty_List);

end SAL.Gen_Unbounded_Definite_Queues;
