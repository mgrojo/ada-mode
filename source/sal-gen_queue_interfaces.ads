--  Abstract:
--
--  Abstract Queue interface.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
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

generic
   type Element_Type is private;
package SAL.Gen_Queue_Interfaces is
   pragma Pure;

   type Queue_Type is limited interface;

   ------------
   --  Dispatching operations on Queue_Type (alphabetical order)

   procedure Clear (Queue : in out Queue_Type) is abstract;
   --  Empty Queue.

   function Count (Queue : in Queue_Type) return Base_Peek_Type is abstract;
   --  Returns count of items in the Queue

   function Length (Queue : in Queue_Type) return Base_Peek_Type renames Count;

   function Is_Empty (Queue : in Queue_Type) return Boolean is abstract;
   --  Returns true if no items are in Queue.

   function Is_Full (Queue : in Queue_Type) return Boolean is abstract;
   --  Returns true if Queue is full.

   function Remove (Queue : in out Queue_Type) return Element_Type is abstract;
   --  Remove head/front item from Queue, return it.
   --
   --  Raises Container_Empty if Is_Empty.

   function Get (Queue : in out Queue_Type) return Element_Type renames Remove;

   procedure Drop (Queue : in out Queue_Type) is abstract;
   --  Remove head/front item from Queue, discard it.
   --
   --  Raises Container_Empty if Is_Empty.

   function Peek (Queue : in Queue_Type; N : Peek_Type := 1) return Element_Type is abstract;
   --  Return a copy of a queue item, without removing it. N = 1 is
   --  the queue head.
   --
   --  Raises Parameter_Error if N > Count

   procedure Add (Queue : in out Queue_Type; Element : in Element_Type) is abstract;
   --  Add Element to the tail/back of Queue.

   procedure Put (Queue : in out Queue_Type; Element : in Element_Type) renames Add;

   procedure Add_To_Head (Queue : in out Queue_Type; Element : in Element_Type) is abstract;
   --  Add Element to the head/front of Queue.

end SAL.Gen_Queue_Interfaces;
