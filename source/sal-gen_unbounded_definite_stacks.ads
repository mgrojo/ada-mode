--  Abstract:
--
--  Stack implementation using Ada.Containers.Vectors.
--
--  Copyright (C) 1998-2000, 2002-2003, 2009, 2015 Stephen Leake.  All Rights Reserved.
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

with Ada.Containers;
with Ada.Finalization;
with SAL.Gen_Stack_Interfaces;
private with Ada.Containers.Vectors;
generic
   type Element_Type is private;
   with package Stack_Interfaces is new SAL.Gen_Stack_Interfaces (Element_Type);

package SAL.Gen_Unbounded_Definite_Stacks is
   pragma Elaborate_Body; -- SAL.Poly.Unbounded_Arrays is.

   type Stack_Type is new Ada.Finalization.Limited_Controlled and Stack_Interfaces.Stack_Type with private;

   overriding procedure Initialize (Stack : in out Stack_Type);

   overriding procedure Finalize (Stack : in out Stack_Type);

   overriding procedure Clear (Stack : in out Stack_Type);

   overriding function Depth (Stack : in Stack_Type) return Ada.Containers.Count_Type;

   overriding function Is_Empty (Stack : in Stack_Type) return Boolean;

   overriding function Max_Depth (Stack : in Stack_Type) return Ada.Containers.Count_Type;

   overriding function Peek (Stack : in Stack_Type; Index : in Natural) return Element_Type;

   overriding procedure Pop (Stack : in out Stack_Type);

   overriding function Pop (Stack : in out Stack_Type) return Element_Type;

   overriding procedure Push (Stack : in out Stack_Type; Item : in Element_Type);

   overriding function Top (Stack : in Stack_Type) return Element_Type;

private

   subtype Positive_Count_Type is Ada.Containers.Count_Type range 1 .. Ada.Containers.Count_Type'Last;

   package Item_Arrays is new Ada.Containers.Vectors
     (Index_Type   => Positive_Count_Type,
      Element_Type => Element_Type);

   type Stack_Type is new Ada.Finalization.Limited_Controlled and Stack_Interfaces.Stack_Type with record
      Top  : Ada.Containers.Count_Type;
      Data : Item_Arrays.Vector;
      --  Top of stack is at Data (Top).
   end record;

end SAL.Gen_Unbounded_Definite_Stacks;
