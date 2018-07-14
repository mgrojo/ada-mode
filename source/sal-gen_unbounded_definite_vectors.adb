--  Abstract :
--
--  See spec.
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

package body SAL.Gen_Unbounded_Definite_Vectors is

   function To_Peek_Type (Item : in Extended_Index) return Peek_Type'Base
   is begin
      return Peek_Type'Base (Item - Index_Type'First) + Peek_Type'First;
   end To_Peek_Type;

   function To_Index_Type (Item : in Peek_Type'Base) return Extended_Index
   is begin
      return Extended_Index (Item - Peek_Type'First) + Index_Type'First;
   end To_Index_Type;

   procedure Grow (Elements : in out Array_Access; Index : in Base_Peek_Type'Base)
   is
      --  Reallocate Elements so Elements (Index) is a valid element.

      Old_First  : constant Peek_Type := Elements'First;
      Old_Last   : constant Peek_Type := Elements'Last;
      New_First  : Peek_Type          := Old_First;
      New_Last   : Peek_Type          := Old_Last;
      New_Length : Peek_Type          := Elements'Length;

      New_Array : Array_Access;
   begin
      loop
         exit when New_First <= Index;
         New_Length := New_Length * 2;
         New_First  := Peek_Type'Max (Peek_Type'First, Old_Last - New_Length + 1);
      end loop;
      loop
         exit when New_Last >= Index;
         New_Length := New_Length * 2;
         New_Last   := Peek_Type'Min (Peek_Type'Last, New_First + New_Length - 1);
      end loop;

      New_Array := new Array_Type (New_First .. New_Last);

      --  We'd like to use this:
      --
      --  New_Array (New_First .. Old_First - 1) := (others => <>);
      --
      --  but that can overflow the stack, since the aggregate is allocated
      --  on the stack.

      for I in New_First .. Old_First - 1 loop
         New_Array (I .. I) := (others => <>);
      end loop;

      New_Array (Old_First .. Old_Last) := Elements.all;

      for I in Old_Last + 1 .. New_Last loop
         New_Array (I .. I)   := (others => <>);
      end loop;

      Free (Elements);
      Elements := New_Array;
   end Grow;

   ----------
   --  public subprograms

   overriding procedure Finalize (Container : in out Vector)
   is begin
      Free (Container.Elements);
      Container.First := No_Index;
      Container.Last  := No_Index;
   end Finalize;

   overriding procedure Adjust (Container : in out Vector)
   is begin
      if Container.Elements /= null then
         Container.Elements := new Array_Type'(Container.Elements.all);
      end if;
   end Adjust;

   overriding function "=" (Left, Right : in Vector) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Left.Length = 0 then
         return Right.Length = 0;
      elsif Left.First /= Right.First then
         return False;
      elsif Left.Last /= Right.Last then
         return False;
      else
         declare
            I : constant Peek_Type := To_Peek_Type (Left.First);
            J : constant Peek_Type := To_Peek_Type (Left.Last);
         begin
            return Left.Elements (I .. J) = Right.Elements (I .. J);
         end;
      end if;
   end "=";

   function Length (Container : in Vector) return Ada.Containers.Count_Type
   is begin
      --  We assume the type ranges are sensible, so no exceptions occur
      --  here.
      if Container.Elements = null then
         return 0;
      else
         return Ada.Containers.Count_Type (To_Peek_Type (Container.Last) - Container.Elements'First + 1);
      end if;
   end Length;

   function Capacity (Container : in Vector) return Ada.Containers.Count_Type
   is begin
      if Container.Elements = null then
         return 0;
      else
         return Ada.Containers.Count_Type (Container.Elements'Length);
      end if;
   end Capacity;

   function Element (Container : Vector; Index : Index_Type) return Element_Type
   is begin
      return Container.Elements (To_Peek_Type (Index));
   end Element;

   procedure Replace_Element (Container : Vector; Index : Index_Type; New_Item : in Element_Type)
   is begin
      Container.Elements (To_Peek_Type (Index)) := New_Item;
   end Replace_Element;

   function First_Index (Container : Vector) return Extended_Index
   is begin
      if Container.Elements = null then
         return No_Index + 1;
      else
         return Container.First;
      end if;
   end First_Index;

   function Last_Index (Container : Vector) return Extended_Index
   is begin
      if Container.Elements = null then
         return No_Index;
      else
         return Container.Last;
      end if;
   end Last_Index;

   procedure Append (Container : in out Vector; New_Item : in Element_Type)
   is begin
      if Container.First = No_Index then
         Container.First := Index_Type'First;
         Container.Last  := Index_Type'First;
      else
         Container.Last := Container.Last + 1;
      end if;

      declare
         J : constant Peek_Type := To_Peek_Type (Container.Last);
      begin
         if Container.Elements = null then
            Container.Elements := new Array_Type (J .. J);

         elsif J > Container.Elements'Last then
            Grow (Container.Elements, J);
         end if;

         Container.Elements (J) := New_Item;
      end;
   end Append;

   procedure Append (Container : in out Vector; New_Items : in Vector)
   is
      use all type Ada.Containers.Count_Type;
      Old_Last : Extended_Index := Container.Last;
   begin
      if New_Items.Length = 0 then
         return;
      end if;

      if Container.First = No_Index then
         Container.First := Index_Type'First;
         Old_Last        := Container.First - 1;
         Container.Last  := Container.First + Extended_Index (New_Items.Length) - 1;
      else
         Container.Last := Container.Last + Extended_Index (New_Items.Length);
      end if;

      declare
         I : constant Peek_Type := To_Peek_Type (Old_Last + 1);
         J : constant Peek_Type := To_Peek_Type (Container.Last);
      begin
         if Container.Elements = null then
            Container.Elements := new Array_Type (I .. J);
         elsif J > Container.Elements'Last then
            Grow (Container.Elements, J);
         end if;

         Container.Elements (I .. J) := New_Items.Elements
           (To_Peek_Type (New_Items.First) .. To_Peek_Type (New_Items.Last));
      end;
   end Append;

   procedure Prepend (Container : in out Vector; New_Item : in Element_Type)
   is begin
      if Container.First = No_Index then
         Container.First := Index_Type'First;
         Container.Last  := Index_Type'First;
      else
         Container.First := Container.First - 1;
      end if;

      declare
         J : constant Peek_Type := To_Peek_Type (Container.First);
      begin
         if Container.Elements = null then
            Container.Elements := new Array_Type (J .. J);

         elsif J < Container.Elements'First then
            Grow (Container.Elements, J);
         end if;

         Container.Elements (J) := New_Item;
      end;
   end Prepend;

   procedure Prepend
     (Target       : in out Vector;
      Source       : in     Vector;
      Source_First : in     Index_Type;
      Source_Last  : in     Index_Type)
   is
      Source_I : constant Peek_Type := To_Peek_Type (Source_First);
      Source_J : constant Peek_Type := To_Peek_Type (Source_Last);
   begin
      if Target.Elements = null then
         Target.Elements := new Array_Type'(Source.Elements (Source_I .. Source_J));
         Target.First    := Source_First;
         Target.Last     := Source_Last;
      else
         declare
            New_First : constant Index_Type := Target.First - (Source_Last - Source_First + 1);
            I         : constant Peek_Type  := To_Peek_Type (New_First);
            J         : constant Peek_Type  := To_Peek_Type (Target.First - 1);
         begin
            if Target.Elements'First > I then
               Grow (Target.Elements, I);
            end if;
            Target.Elements (I .. J) := Source.Elements (Source_I .. Source_J);
            Target.First := New_First;
         end;
      end if;
   end Prepend;

   procedure Insert
     (Container : in out Vector;
      Element   : in     Element_Type;
      Before    : in     Index_Type)
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Container.Length = 0 then
         Container.Append (Element);
      else
         Container.Last := Container.Last + 1;

         declare
            J : constant Peek_Type := To_Peek_Type (Before);
            K : constant Peek_Type := To_Peek_Type (Container.Last);
         begin
            if K > Container.Elements'Last then
               Grow (Container.Elements, K);
            end if;

            Container.Elements (J + 1 .. K) := Container.Elements (J .. K - 1);
            Container.Elements (J) := Element;
         end;
      end if;
   end Insert;

   procedure Merge
     (Target : in out Vector;
      Source : in out Vector)
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Source.Length = 0 then
         Source.Clear;

      elsif Target.Length = 0 then
         Target := Source;
         Source.Clear;

      else
         declare
            New_First : constant Index_Type := Extended_Index'Min (Target.First, Source.First);
            New_Last  : constant Index_Type := Extended_Index'Max (Target.Last, Source.Last);
            New_I     : constant Peek_Type  := To_Peek_Type (New_First);
            New_J     : constant Peek_Type  := To_Peek_Type (New_Last);
         begin
            if New_I < Target.Elements'First then
               Grow (Target.Elements, New_I);
            end if;
            if New_J > Target.Elements'Last then
               Grow (Target.Elements, New_J);
            end if;

            Target.Elements (To_Peek_Type (Source.First) .. To_Peek_Type (Source.Last)) := Source.Elements
              (To_Peek_Type (Source.First) .. To_Peek_Type (Source.Last));

            Target.First := New_First;
            Target.Last  := New_Last;

            Source.Clear;
         end;
      end if;
   end Merge;

   function To_Vector (Item : in Element_Type; Count : in Ada.Containers.Count_Type := 1) return Vector
   is begin
      return Result : Vector do
         for I in 1 .. Count loop
            Result.Append (Item);
         end loop;
      end return;
   end To_Vector;

   function "+" (Element : in Element_Type) return Vector
   is begin
      return Result : Vector do
         Result.Append (Element);
      end return;
   end "+";

   function "&" (Left, Right : in Element_Type) return Vector
   is begin
      return Result : Vector do
         Result.Append (Left);
         Result.Append (Right);
      end return;
   end "&";

   function "&" (Left : in Vector; Right : in Element_Type) return Vector
   is begin
      return Result : Vector := Left do
         Result.Append (Right);
      end return;
   end "&";

   procedure Set_First (Container : in out Vector; First : in Index_Type)
   is
      J : constant Peek_Type := To_Peek_Type (First);
   begin
      Container.First := First;
      if Container.Last = No_Index then
         Container.Last := First - 1;
      end if;

      if Container.Last >= First then
         if Container.Elements = null then
            Container.Elements := new Array_Type'(J .. To_Peek_Type (Container.Last) => <>);

         elsif Container.Elements'First > J then
            Grow (Container.Elements, J);
         end if;
      end if;
   end Set_First;

   procedure Set_Last (Container : in out Vector; Last : in Index_Type)
   is
      J : constant Peek_Type := To_Peek_Type (Last);
   begin
      Container.Last := Last;
      if Container.First = No_Index then
         Container.First := Last + 1;
      end if;

      if Last >= Container.First then
         if Container.Elements = null then
            Container.Elements := new Array_Type'(To_Peek_Type (Container.First) .. J => <>);

         elsif Container.Elements'Last < J then
            Grow (Container.Elements, J);
         end if;
      end if;
   end Set_Last;

   procedure Set_Length (Container : in out Vector; Length : in Ada.Containers.Count_Type)
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Container.First = No_Index then
         Container.First := Index_Type'First;
         Container.Last  := Container.First - 1;
      end if;
      if Length > 0 then
         Container.Set_Last (Index_Type (Length) + Container.First - 1);
      end if;
   end Set_Length;

   procedure Set_Length
     (Container : in out Vector;
      Length    : in     Ada.Containers.Count_Type;
      Default   : in     Element_Type)
   is
      Old_First : constant Extended_Index := Container.First;
      Old_Last  : constant Extended_Index := Container.Last;
   begin
      Set_Length (Container, Length);
      if Old_First = No_Index then
         Container.Elements.all := (others => Default);
      else
         Container.Elements (To_Peek_Type (Old_Last + 1) .. To_Peek_Type (Container.Last)) := (others => Default);
      end if;
   end Set_Length;

   procedure Delete (Container : in out Vector; Index : in Index_Type)
   is
      J : constant Peek_Type := To_Peek_Type (Index);
   begin
      Container.Elements (J .. J) := (J => <>);
      if Index = Container.Last then
         Container.Last := Container.Last - 1;
      end if;
   end Delete;

   function Contains (Container : in Vector; Element : in Element_Type) return Boolean
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Container.Length = 0 then
         return False;
      else
         for It of Container.Elements.all loop
            if It = Element then
               return True;
            end if;
         end loop;
         return False;
      end if;
   end Contains;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position.Index /= Invalid_Peek_Index;
   end Has_Element;

   function Element (Position : Cursor) return Element_Type
   is begin
      return Position.Container.Elements (Position.Index);
   end Element;

   function First (Container : aliased in Vector) return Cursor
   is
      use all type Ada.Containers.Count_Type;
   begin
      if Container.Length = 0 then
         return No_Element;
      else
         return (Container'Access, To_Peek_Type (Container.First));
      end if;
   end First;

   function Next (Position : in Cursor) return Cursor
   is begin
      if Position = No_Element then
         return No_Element;
      elsif Position.Index < To_Peek_Type (Position.Container.Last) then
         return (Position.Container, Position.Index + 1);
      else
         return No_Element;
      end if;
   end Next;

   procedure Next (Position : in out Cursor)
   is begin
      if Position = No_Element then
         null;
      elsif Position.Index < To_Peek_Type (Position.Container.Last) then
         Position.Index := Position.Index + 1;
      else
         Position := No_Element;
      end if;
   end Next;

   function To_Cursor
     (Container : aliased in Vector;
      Index     :         in Extended_Index)
     return Cursor
   is begin
      if Index not in Container.First .. Container.Last then
         return No_Element;
      else
         return (Container'Access, To_Peek_Type (Index));
      end if;
   end To_Cursor;

   function To_Index (Position : in Cursor) return Extended_Index
   is begin
      if Position = No_Element then
         return No_Index;
      else
         return To_Index_Type (Position.Index);
      end if;
   end To_Index;

   function Constant_Ref (Container : aliased Vector; Index : in Index_Type) return Constant_Reference_Type
   is
      J : constant Peek_Type := To_Peek_Type (Index);
   begin
      return (Element => Container.Elements (J)'Access);
   end Constant_Ref;

   function Variable_Ref
     (Container : aliased in Vector;
      Index     :         in Index_Type)
     return Variable_Reference_Type
   is
      J : constant Peek_Type := To_Peek_Type (Index);
   begin
      return (Element => Container.Elements (J)'Access);
   end Variable_Ref;

   overriding function First (Object : Iterator) return Cursor
   is begin
      if Object.Container.Elements = null then
         return (null, Invalid_Peek_Index);
      else
         return (Object.Container, To_Peek_Type (Object.Container.First_Index));
      end if;
   end First;

   overriding function Last  (Object : Iterator) return Cursor
   is begin
      if Object.Container.Elements = null then
         return (null, Invalid_Peek_Index);
      else
         return (Object.Container, To_Peek_Type (Object.Container.Last_Index));
      end if;
   end Last;

   overriding function Next (Object : in Iterator; Position : in Cursor) return Cursor
   is begin
      if Position.Index = To_Peek_Type (Object.Container.Last) then
         return (null, Invalid_Peek_Index);
      else
         return (Object.Container, Position.Index + 1);
      end if;
   end Next;

   overriding function Previous (Object : in Iterator; Position : in Cursor) return Cursor
   is begin
      if Position.Index = To_Peek_Type (Index_Type'First) then
         return (null, Invalid_Peek_Index);
      else
         return (Object.Container, Position.Index - 1);
      end if;
   end Previous;

   function Iterate (Container : aliased in Vector) return Iterator_Interfaces.Reversible_Iterator'Class
   is begin
      return Iterator'(Container => Container'Unrestricted_Access);
   end Iterate;

   function Constant_Ref (Container : aliased Vector; Position : in Cursor) return Constant_Reference_Type
   is begin
      return (Element => Container.Elements (Position.Index)'Access);
   end Constant_Ref;

   function Variable_Ref
     (Container : aliased in Vector;
      Position  :         in Cursor)
     return Variable_Reference_Type
   is begin
      return (Element => Container.Elements (Position.Index)'Access);
   end Variable_Ref;

end SAL.Gen_Unbounded_Definite_Vectors;
