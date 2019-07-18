--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 - 2019 Free Software Foundation, Inc.
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

package body SAL.Gen_Bounded_Definite_Vectors_Sorted is

   function Length (Container : in Vector) return Ada.Containers.Count_Type
   is begin
      return Ada.Containers.Count_Type (Container.Last);
   end Length;

   function Is_Full (Container : in Vector) return Boolean
   is begin
      return Container.Last = Peek_Type (Capacity);
   end Is_Full;

   procedure Clear (Container : in out Vector)
   is begin
      Container.Last := No_Index;
   end Clear;

   procedure Insert
     (Container       : in out Vector;
      New_Item        : in     Element_Type;
      Ignore_If_Equal : in     Boolean := False)
   is
      K : constant Base_Peek_Type := Container.Last;
      J : Base_Peek_Type := K;
   begin
      if K + 1 > Container.Elements'Last then
         raise Container_Full;

      elsif K = 0 then
         --  Container empty
         Container.Last := Container.Last + 1;
         Container.Elements (1) := New_Item;
         return;
      end if;

      loop
         exit when J < 1;

         case Element_Compare (New_Item, Container.Elements (J)) is
         when Less =>
            J := J - 1;
         when Equal =>
            if Ignore_If_Equal then
               return;
            else
               --  Insert after J
               exit;
            end if;
         when Greater =>
            --  Insert after J
            exit;
         end case;
      end loop;

      if J = 0 then
         --  Insert before all
         Container.Elements (2 .. K + 1) := Container.Elements (1 .. K);
         Container.Elements (1) := New_Item;
      else
         --  Insert after J
         Container.Elements (J + 2 .. K + 1) := Container.Elements (J + 1 .. K);
         Container.Elements (J + 1) := New_Item;
      end if;
      Container.Last := Container.Last + 1;
   end Insert;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Container = null then
         return False;
      end if;

      return Position.Index <= Position.Container.Last;
   end Has_Element;

   overriding function First (Object : Iterator) return Cursor
   is begin
      if Object.Container.Last = No_Index then
         return (null, No_Index);
      else
         return (Object.Container, Peek_Type'First);
      end if;
   end First;

   overriding function Last  (Object : Iterator) return Cursor
   is begin
      if Object.Container.Last = No_Index then
         return (null, No_Index);
      else
         return (Object.Container, Object.Container.Last);
      end if;
   end Last;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is begin
      if Position.Index = Object.Container.Last then
         return (null, No_Index);
      else
         return (Object.Container, Position.Index + 1);
      end if;
   end Next;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is begin
      if Position.Index = Peek_Type'First then
         return (null, No_Index);
      else
         return (Object.Container, Position.Index - 1);
      end if;
   end Previous;

   function Iterate (Container : Vector) return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is begin
      return Iterator'
        (Container => Container'Unrestricted_Access,
         Index     => No_Index);
   end Iterate;

   function Constant_Reference (Container : aliased Vector; Position : in Cursor) return Constant_Reference_Type
   is begin
      return (Element => Container.Elements (Position.Index)'Access);
   end Constant_Reference;

   function Last_Index (Container : aliased Vector) return Base_Peek_Type
   is begin
      return Container.Last;
   end Last_Index;

   function Constant_Reference (Container : aliased Vector; Index : in Peek_Type) return Constant_Reference_Type
   is begin
      return (Element => Container.Elements (Index)'Access);
   end Constant_Reference;

end SAL.Gen_Bounded_Definite_Vectors_Sorted;
