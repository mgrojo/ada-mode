--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.Parser.LR.AUnit is

   function To_State_Stack (Item : in State_Array) return State_Stacks.Stack_Type
   is begin
      return
        Result : State_Stacks.Stack_Type
      do
         Result.Set_Depth (Item'Length);
         for I in State_Stack_Interfaces.Positive_Count_Type'(1) .. Item'Length loop
            Result.Set (I, Item'Length, Item (I));
         end loop;
      end return;
   end To_State_Stack;

end WisiToken.Parser.LR.AUnit;
