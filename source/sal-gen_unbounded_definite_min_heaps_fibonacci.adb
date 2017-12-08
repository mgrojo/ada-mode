--  Abstract:
--
--  See spec.
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

with Ada.Unchecked_Deallocation;
with Long_Float_Elementary_Functions;
package body SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci is

   ----------
   --  local subprogram specs (as needed), alphabetical order

   procedure Insert_Into_Root_List (Heap : in out Heap_Type; X : in Node_Access);

   procedure Link (Y, X : in Node_Access);

   procedure Remove_From_List (X : in Node_Access);

   procedure Swap (A, B : in out Node_Access);

   ----------
   --  local subprogram bodies, alphabetical order

   procedure Consolidate (Heap : in out Heap_Type)
   is
      --  [1] 19.4 max degree of Fibonacci heap
      Phi : constant := 1.61803398874989484820458683436563811772; -- https://oeis.org/A001622/constant
      Max_Degree : constant Integer := Integer
        (Long_Float_Elementary_Functions.Log (Long_Float (Heap.Count), Base => Phi));

      --  [1] 19.2 CONSOLIDATE
      A : array (0 .. Max_Degree) of Node_Access := (others => null);

      W    : Node_Access := Heap.Min;
      Last : Node_Access := Heap.Min;
      X, Y : Node_Access;
      D    : Integer;

      Min_Key : Key_Type;
   begin
      loop
         X := W;
         W := W.Right;

         D := X.Degree;

         loop
            exit when A (D) = null;

            Y := A (D);
            if Key (Y.Element) < Key (X.Element) then
               Swap (X, Y);
            end if;
            if Y = Last and W /= Last then
               Last := Y.Right;
            end if;
            Link (Y, X);
            A (D) := null;
            D := D + 1;
            exit when D = A'Last;
         end loop;

         A (D) := X;

         exit when W = Last;
      end loop;

      Heap.Min := null;
      for I in A'Range loop
         if A (I) /= null then
            if Heap.Min = null then
               Heap.Min       := A (I);
               Heap.Min.Left  := Heap.Min;
               Heap.Min.Right := Heap.Min;
               Min_Key        := Key (Heap.Min.Element);
            else
               Insert_Into_Root_List (Heap, A (I));
               if Key (A (I).Element) < Min_Key then
                  Heap.Min := A (I);
                  Min_Key := Key (A (I).Element);
               end if;
            end if;
         end if;
      end loop;
   end Consolidate;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   procedure Free_Node (Item : in out Node_Access)
   is
      Child : Node_Access;
      Temp  : Node_Access;
   begin
      if Item = null then
         return;
      end if;

      --  Parent has already been free'd
      --  Siblings are freed by caller

      --  Free children
      if Item.Child /= null then
         Child := Item.Child;

         loop
            Temp  := Child;
            Child := Child.Right;
            Free_Node (Temp);
            exit when Child = Item.Child;
         end loop;
      end if;
      Free (Item);
   end Free_Node;

   procedure Insert_Into_Root_List (Heap : in out Heap_Type; X : in Node_Access)
   is begin
      --  match [1] fig 19.3
      X.Right             := Heap.Min;
      X.Left              := Heap.Min.Left;
      Heap.Min.Left.Right := X;
      Heap.Min.Left       := X;
   end Insert_Into_Root_List;

   procedure Link (Y, X : in Node_Access)
   is begin
      --  [1] 19.2 FIB-HEAP-LINK
      Remove_From_List (Y);
      Y.Parent := X;
      X.Degree := X.Degree + 1;
      if X.Child = null then
         X.Child := Y;
         Y.Right := Y;
         Y.Left  := Y;
      else
         --  Insert Y into X child list
         Y.Right            := X.Child;
         Y.Left             := X.Child.Left;
         X.Child.Left.Right := Y;
         X.Child.Left       := Y;
      end if;
      Y.Mark := False;
   end Link;

   procedure Remove_From_List (X : in Node_Access)
   is begin
      X.Left.Right := X.Right;
      X.Right.Left := X.Left;
   end Remove_From_List;

   procedure Swap (A, B : in out Node_Access)
   is
      C : constant Node_Access := A;
   begin
      A := B;
      B := C;
   end Swap;

   ----------
   --  Visible operations

   overriding
   procedure Initialize (Object : in out Heap_Type)
   is begin
      --  Min is null by default.
      Object.Count := 0;
   end Initialize;

   overriding
   procedure Finalize (Object : in out Heap_Type)
   is
      Next : Node_Access := Object.Min;
      Temp : Node_Access;
   begin
      if Next = null then
         return;
      end if;

      loop
         Temp := Next;
         Next := Next.Right;
         Free_Node (Temp);
         exit when Next = Object.Min;
      end loop;
      Object.Count := 0;
   end Finalize;

   procedure Clear (Heap : in out Heap_Type)
   is begin
      Heap.Count := 0;
   end Clear;

   function Count (Heap : in Heap_Type) return Base_Peek_Type
   is begin
      return Heap.Count;
   end Count;

   function Remove (Heap : in out Heap_Type) return Element_Type
   is
      Z           : Node_Access := Heap.Min;
      Child, Temp : Node_Access;
   begin
      if Heap.Count = 0 then
         raise Container_Empty;
      end if;

      --  [1] 19.2 FIB-HEAP-EXTRACT-MIN
      Child := Z.Child;
      for I in 1 .. Z.Degree loop
         Temp        := Child;
         Child       := Child.Right;
         Temp.Parent := null;
         Insert_Into_Root_List (Heap, Temp);
      end loop;

      Remove_From_List (Z);

      if Z.Right = Z then
         Heap.Min := null;
      else
         Heap.Min := Z.Right;
         Consolidate (Heap);
      end if;
      Heap.Count := Heap.Count - 1;

      return Result : constant Element_Type := Z.Element do
         Free (Z);
      end return;
   end Remove;

   function Min_Key (Heap : in out Heap_Type) return Key_Type
   is begin
      return Key (Heap.Min.Element);
   end Min_Key;

   procedure Drop (Heap : in out Heap_Type)
   is
      Junk : Element_Type := Remove (Heap);
      pragma Unreferenced (Junk);
   begin
      null;
   end Drop;

   procedure Add (Heap : in out Heap_Type; Item : in Element_Type)
   is
      X : constant Node_Access := new Node'(Item, null, null, null, null, 0, False);
   begin
      --  [1] 19.2 FIB-HEAP-INSERT
      if Heap.Min = null then
         Heap.Min       := X;
         Heap.Min.Left  := Heap.Min;
         Heap.Min.Right := Heap.Min;
      else
         Insert_Into_Root_List (Heap, X);

         if Key (Item) < Key (Heap.Min.Element) then
            Heap.Min := X;
         end if;
      end if;
      Heap.Count := Heap.Count + 1;
   end Add;

end SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci;
