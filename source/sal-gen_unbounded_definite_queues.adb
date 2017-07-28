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

package body SAL.Gen_Unbounded_Definite_Queues is

   overriding procedure Clear (Queue : in out Queue_Type)
   is begin
      Queue.Data.Clear;
   end Clear;

   overriding function Count (Queue : in Queue_Type) return Ada.Containers.Count_Type
   is begin
      return Queue.Data.Length;
   end Count;

   overriding function Is_Empty (Queue : in Queue_Type) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      return Queue.Data.Length = 0;
   end Is_Empty;

   overriding function Remove (Queue : in out Queue_Type) return Element_Type
   is
      use Element_Lists;
   begin
      return A : constant Element_Type := Element (Queue.Data.First) do
         Queue.Data.Delete_First;
      end return;
   end Remove;

   overriding procedure Drop (Queue : in out Queue_Type)
   is begin
      Queue.Data.Delete_First;
   end Drop;

   overriding function Peek (Queue : in Queue_Type; N : Ada.Containers.Count_Type := 0) return Element_Type
   is
      use Element_Lists;
      use all type Ada.Containers.Count_Type;
      I : Cursor := Queue.Data.First;
   begin
      if N > Queue.Data.Length then
         raise Parameter_Error;
      end if;

      for K in 1 .. N loop
         Next (I);
      end loop;

      return Element (I);
   end Peek;

   overriding procedure Add (Queue : in out Queue_Type; Item : in Element_Type)
   is begin
      Queue.Data.Append (Item);
   end Add;

   overriding procedure Add_To_Head (Queue : in out Queue_Type; Item : in Element_Type)
   is begin
      Queue.Data.Prepend (Item);
   end Add_To_Head;

   overriding procedure Add
     (Queue   : in out Queue_Type;
      Element : in     Element_Type;
      Order   : in     Queue_Interfaces.Order_Type)
   is
      use Element_Lists;

      function ">" (A, B : in Element_Type) return Boolean renames Order.all;
      --  Could also be "<"
      --
      --  Remove removes First, so if Order is ">", then a typical list is (6, 5, 4).
      --  If Order is "<", (1, 2, 3).

      I : Cursor := Queue.Data.First;
   begin
      if I = No_Element then
         Queue.Data.Append (Element);

      elsif Element > Queue.Data (I) then
         --  example >: insert 7 into (6, 5, 4)
         --  example <: insert 1 into (2, 3, 4)
         Queue.Data.Prepend (Element);

      else
         loop
            Next (I);
            exit when I = No_Element;
            exit when Element > Queue.Data (I);
         end loop;

         if I = No_Element then
            --  example >: insert 3 into (6, 5, 4)
            --  example <: insert 3 into (0, 1, 2)
            Queue.Data.Append (Element);
         else
            --  example >: insert 5 into (3, 4, 6)
            --  example <: insert 1 into (2, 0)
            Next (I);
            Queue.Data.Insert (Before => I, New_Item => Element);
         end if;
      end if;
   end Add;

end SAL.Gen_Unbounded_Definite_Queues;
