--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2004, 2017 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

with SAL.AUnit;
package body SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci.Gen_Test is

   type Cursor is record
      --  Every node is in a circular list. List_Origin is the node where we
      --  entered the list, so we know when we are done.
      Node        : Node_Access;
      List_Origin : Node_Access;
   end record;

   function Element (Cur : in Cursor) return Element_Type
   is begin
      return Cur.Node.Element;
   end Element;

   procedure Check
     (Label    : in     String;
      Cur      : in out Cursor;
      Expected : in     Element_Array_Type;
      I        : in out Base_Peek_Type)
   is
      Next_Cur : Cursor;
   begin
      loop
         if Cur.Node.Child /= null then
            Next_Cur := (Cur.Node.Child, Cur.Node.Child);
            Check (Label, Next_Cur, Expected, I);
         end if;
         Check (Label & SAL.Base_Peek_Type'Image (I), Element (Cur), Expected (I));
         I := I + 1;
         Cur.Node := Cur.Node.Right;
         exit when Cur.Node = Cur.List_Origin;
      end loop;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Heap_Type;
      Expected : in Element_Array_Type)
   is
      use SAL.AUnit;
      I   : Base_Peek_Type := Expected'First;
      Cur : Cursor         := (Computed.Min, Computed.Min);
   begin
      Check (Label & ".count", Computed.Count, Expected'Length);
      if Computed.Min = null then
         Check (Label & ".count = 0", Computed.Count, 0);
      else
         Check (Label, Cur, Expected, I);
      end if;
   end Check;

   function Max_Degree (Heap : in Heap_Type) return Integer
   is
      N      : Node_Access := Heap.Min;
      Result : Integer     := Heap.Min.Degree;
   begin
      loop
         if N.Degree > Result then
            Result := N.Degree;
         end if;
         N := N.Right;
         exit when N = Heap.Min;
      end loop;
      return Result;
   end Max_Degree;

end SAL.Gen_Unbounded_Definite_Min_Heaps_Fibonacci.Gen_Test;
