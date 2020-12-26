--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

package body SAL.Gen_Unbounded_Sparse_Ordered_Sets is

   procedure Clear (Set : in out Pkg.Set)
   is begin
      Set.Tree.Finalize;
      Set.Tree.Initialize;
   end Clear;

   function Count (Set : in Pkg.Set) return Ada.Containers.Count_Type
   is
      use Ada.Containers;
      Result : Count_Type := 0;
   begin
      for N of Set loop
         Result := @ + 1;
      end loop;
      return Result;
   end Count;

   procedure Insert (Set : in out Pkg.Set; Item : in Index_Type)
   is begin
      Set.Tree.Insert (Item, Duplicate => Ignore);
   end Insert;

   function Contains (Set : in Pkg.Set; Item : in Index_Type) return Boolean
   is begin
      return Boolean_Trees.Has_Element (Set.Tree.Find (Item));
   end Contains;

   procedure Delete (Set : in out Pkg.Set; Item : in Index_Type)
   is begin
      Set.Tree.Delete (Item);
   end Delete;

   function Has_Element (Position : in Cursor) return Boolean
   is begin
      return Boolean_Trees.Has_Element (Position.Cur);
   end Has_Element;

   function Element (Position : in Cursor) return Index_Type
   is begin
      return Boolean_Trees.Key (Position.Cur);
   end Element;

   function Constant_Ref
     (Container : aliased in Set;
      Position  :         in Cursor)
     return Constant_Reference_Type
   is begin
      return (Element => Container.Tree.Constant_Ref (Position.Cur).Element, Dummy => 0);
   end Constant_Ref;

   function Iterate (Container : aliased in Pkg.Set'Class) return Iterator
   is
      --  Let Initialize call Tree.Iterate; calling it directly here gives
      --  accessibility error.
      Iter_1 : Wrapped_Boolean_Iterator (Container'Access);
   begin
      return
        (Container => Container'Access,
         Iter_1    => Iter_1);
   end Iterate;

   overriding function First (Iterator : in Pkg.Iterator) return Cursor
   is begin
      return (Cur => Iterator.Iter_1.Iter_2.First);
   end First;

   overriding function Next (Iterator : in Pkg.Iterator; Position : in Cursor) return Cursor
   is begin
      return (Cur => Iterator.Iter_1.Iter_2.Next (Position.Cur));
   end Next;

   overriding procedure Initialize (Iter : in out Wrapped_Boolean_Iterator)
   is begin
      Iter.Iter_2 := new Boolean_Trees.Iterator'(Iter.Container.Tree.Iterate);
   end Initialize;

   overriding procedure Finalize (Iter : in out Wrapped_Boolean_Iterator)
   is begin
      Free (Iter.Iter_2);
   end Finalize;

   overriding procedure Adjust (Iter : in out Wrapped_Boolean_Iterator)
   is begin
      Iter.Iter_2 := new Boolean_Trees.Iterator'(Iter.Container.Tree.Iterate);
   end Adjust;

end SAL.Gen_Unbounded_Sparse_Ordered_Sets;
