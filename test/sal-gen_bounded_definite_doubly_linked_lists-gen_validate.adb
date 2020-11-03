--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2020 Stephen Leake.  All Rights Reserved.
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
package body SAL.Gen_Bounded_Definite_Doubly_Linked_Lists.Gen_Validate is

   procedure Validate (Label : in String; Container : in List)
   is
      use AUnit.Assertions;
      I : Base_Peek_Type;
      J : Integer := 1;

      Present : array (Peek_Type range 1 .. Container.Size) of Boolean := (others => False);
   begin
      if Container.Head = Invalid_Peek_Index then
         Assert (Container.Tail = Invalid_Peek_Index, Label & ": head, tail not both Invalid_Peek_Index");

         Assert (Container.Free_Last = Container.Size, Label & ": free_last /= size");

      else

         Assert (Container.Nodes (Container.Head).Prev = Invalid_Peek_Index,
                 Label & ": head.prev /= Invalid_Peek_Index");

         Assert (Container.Nodes (Container.Tail).Next = Invalid_Peek_Index,
                 Label & ": tail.next /= Invalid_Peek_Index");

         I := Container.Head;
         Test_Elements :
         loop
            declare
               Node : Node_Type renames Container.Nodes (I);
            begin
               Present (I) := True;

               if Node.Next = Invalid_Peek_Index then
                  Assert (Container.Tail = I, Label & Integer'Image (J) & ": tail not last item");
                  exit Test_Elements;
               else
                  Assert (Container.Nodes (Node.Next).Prev = I, Label & Integer'Image (J) & ": next.prev /= current");
               end if;

               I := Node.Next;
               J := J + 1;
            end;
         end loop Test_Elements;
      end if;

      for I in 1 .. Container.Free_Last loop
         Present (Container.Free_List (I)) := True;
      end loop;

      Assert ((for all P of Present => P), Label & ": some node not referenced in Free_List or list");
   end Validate;

end SAL.Gen_Bounded_Definite_Doubly_Linked_Lists.Gen_Validate;
