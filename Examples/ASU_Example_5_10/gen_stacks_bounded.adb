--  Abstract:
--
--  see spec
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This program is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this program; see file COPYING. If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

pragma License (GPL);

package body Gen_Stacks_Bounded is

   ------------
   --  Operations on Stack_Type (alphabetical order)

   procedure Clear (Stack : in out Stack_Type)
   is begin
      Stack.Top := 0;
   end Clear;

   function Depth (Stack : in Stack_Type) return Natural
   is begin
      return Stack.Top;
   end Depth;

   function Is_Empty (Stack : in Stack_Type) return Boolean
   is begin
      return Stack.Top = 0;
   end Is_Empty;

   function Is_Full (Stack : in Stack_Type) return Boolean
   is begin
      return Stack.Top = Stack.Size;
   end Is_Full;

   function Peek (Stack : in Stack_Type; Index : in Positive) return Item_Type
   is begin
      return Stack.Items (Stack.Top - Index + 1);
   end Peek;

   procedure Pop (Stack : in out Stack_Type)
   is begin
      if Is_Empty (Stack) then
         raise Container_Empty;
      else
         Stack.Top := Stack.Top - 1;
      end if;
   end Pop;

   procedure Push (Stack : in out Stack_Type; Item : in Item_Type)
   is begin
      if Is_Full (Stack) then
         raise Container_Full;
      else
         Stack.Top := Stack.Top + 1;
         Stack.Items (Stack.Top) := Item;
      end if;
   end Push;

   function Top (Stack : in Stack_Type) return Item_Type
   is begin
      if Is_Empty (Stack) then
         raise Container_Empty;
      else
         return Stack.Items (Stack.Top);
      end if;
   end Top;

end Gen_Stacks_Bounded;
