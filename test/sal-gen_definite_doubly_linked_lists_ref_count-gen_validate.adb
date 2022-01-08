--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2017, 2018, 2021, 2022 Stephen Leake.  All Rights Reserved.
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

pragma License (GPL);

with AUnit.Assertions;
with AUnit.Checks;
package body SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count.Gen_Validate is

   procedure Validate (Label : in String; Container : in List)
   is
      use all type Ada.Containers.Count_Type;
      use AUnit.Assertions;
      I : Node_Access;
      J : Integer := 1;
   begin
      if Container.Head = null or Container.Tail = null or Container.Count = 0 then
         Assert (Container.Head = null, Label & ": head /= null");
         Assert (Container.Tail = null, Label & ": tail /= null");
         Assert (Container.Count = 0, Label & ": count /= null");

      else
         Assert (Container.Head.Prev = null, Label & ": head.prev /= null");
         Assert (Container.Tail.Next = null, Label & ": tail.next /= null");

         I := Container.Head;
         Test_Elements :
         loop
            if I.Next = null then
               Assert (Container.Tail = I, Label & Integer'Image (J) & ": tail not last item");
               exit Test_Elements;
            else
               Assert (I.Next.Prev = I, Label & Integer'Image (J) & ": next.prev /= current");
            end if;
            I := I.Next;
            J := J + 1;
         end loop Test_Elements;
      end if;
   end Validate;

   procedure Check is new AUnit.Checks.Gen_Check_Access (Node_Type, Node_Access);

   procedure Check
     (Label    : in String;
      Computed : in Cursor;
      Expected : in Cursor)
   is begin
      Check (Label, Computed.Ptr, Expected.Ptr);
   end Check;

   procedure Check_Ref_Counts
     (Label      : in String;
      Container  : in List;
      Ref_Counts : in Ref_Counts_Array)
   is
      use AUnit.Checks;

      Node : Node_Access := Container.Head;
   begin
      Check (Label & ".length", Integer (Container.Length), Ref_Counts'Length);
      for I in Ref_Counts'Range loop
         Check (Label & "." & I'Image, Node.Ref_Count, Ref_Counts (I));
         Node := Node.Next;
      end loop;
   end Check_Ref_Counts;

end SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count.Gen_Validate;
