--  Abstract:
--
--  see spec
--
--  Copyright (C) 1998, 2003, 2009, 2015, 2017 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception, if other files instantiate generics from
--  SAL, or you link SAL object files with other files to produce an
--  executable, that does not by itself cause the resulting executable
--  to be covered by the GNU General Public License. This exception
--  does not however invalidate any other reasons why the executable
--  file might be covered by the GNU Public License.

pragma License (Modified_GPL);

package body SAL.Gen_Unbounded_Definite_Stacks is

   overriding procedure Clear (Stack : in out Stack_Type)
   is
      use Element_Arrays;
   begin
      --  We don't change the reserved capacity, on the assumption the
      --  stack will be used again.
      for I in 1 .. Stack.Data.Length loop
         Stack.Data.Delete_Last;
      end loop;
      Stack.Top := 0;
   end Clear;

   overriding function Depth (Stack : in Stack_Type) return Ada.Containers.Count_Type
   is begin
      return Stack.Top;
   end Depth;

   overriding function Is_Empty (Stack : in Stack_Type) return Boolean
   is
      use type Ada.Containers.Count_Type;
   begin
      return Stack.Top = 0;
   end Is_Empty;

   overriding function Max_Depth (Stack : in Stack_Type) return Ada.Containers.Count_Type
   is
      use type Ada.Containers.Count_Type;
   begin
      return Stack.Data.Last_Index;
   end Max_Depth;

   overriding function Peek
     (Stack : in Stack_Type;
      Index : in Stack_Interfaces.Positive_Count_Type := 1)
     return Element_Type
   is
      use Ada.Containers;
   begin
      return Element_Arrays.Element (Stack.Data, Stack.Top - Index + 1);
   end Peek;

   overriding procedure Pop (Stack : in out Stack_Type)
   is
      use Ada.Containers;
   begin
      if Stack.Top = 0 then
         raise Container_Empty;
      else
         Stack.Data.Delete (Stack.Top);
         Stack.Top := Stack.Top - 1;
      end if;
   end Pop;

   overriding function Pop (Stack : in out Stack_Type) return Element_Type
   is
      use type Ada.Containers.Count_Type;
   begin
      if Stack.Top = 0 then
         raise Container_Empty;
      else
         return Result : constant Element_Type := Stack.Peek (1)
         do
            Stack.Pop;
         end return;
      end if;
   end Pop;

   overriding procedure Push (Stack : in out Stack_Type; Item : in Element_Type)
   is
      use Ada.Containers;
   begin
      Stack.Top       := Stack.Top + 1;
      Stack.Data.Reserve_Capacity (Stack.Top);
      if Stack.Top > Stack.Data.Last_Index then
         Stack.Data.Append (Item);
      else
         Stack.Data.Replace_Element (Stack.Top, Item);
      end if;
   end Push;

   overriding function Top (Stack : in Stack_Type) return Element_Type
   is
      use type Ada.Containers.Count_Type;
   begin
      if Stack.Top < 1 then
         raise SAL.Container_Empty;
      else
         return Peek (Stack, 1);
      end if;
   end Top;

end SAL.Gen_Unbounded_Definite_Stacks;
