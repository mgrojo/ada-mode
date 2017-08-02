--  Abstract:
--
--  Stack implementation using Ada.Containers.Vectors.
--
--  Copyright (C) 1998-2000, 2002-2003, 2009, 2015, 2017 Stephen Leake.  All Rights Reserved.
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
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers;
with SAL.Gen_Stack_Interfaces;
private with Ada.Containers.Vectors;
generic
   type Element_Type is private;
   with package Stack_Interfaces is new SAL.Gen_Stack_Interfaces (Element_Type);
package SAL.Gen_Unbounded_Definite_Stacks is

   type Stack_Type is new Stack_Interfaces.Stack_Type with private;

   overriding procedure Clear (Stack : in out Stack_Type);

   overriding function Depth (Stack : in Stack_Type) return Ada.Containers.Count_Type;

   overriding function Is_Empty (Stack : in Stack_Type) return Boolean;

   overriding function Max_Depth (Stack : in Stack_Type) return Ada.Containers.Count_Type;

   overriding function Peek
     (Stack : in Stack_Type;
      Index : in Stack_Interfaces.Positive_Count_Type := 1)
     return Element_Type;

   overriding procedure Pop (Stack : in out Stack_Type);

   overriding function Pop (Stack : in out Stack_Type) return Element_Type;

   overriding procedure Push (Stack : in out Stack_Type; Item : in Element_Type);

   overriding function Top (Stack : in Stack_Type) return Element_Type;

   --  Other operations

   Empty_Stack : constant Stack_Type;

   procedure Set_Depth
     (Stack : in out Stack_Type;
      Depth : in     Stack_Interfaces.Positive_Count_Type);
   --  Empty Stack, set its Depth to Depth. Must be followed by Set
   --  for each element.
   --
   --  Useful when creating a stack from pre-existing data.

   procedure Set
     (Stack   : in out Stack_Type;
      Index   : in     Stack_Interfaces.Positive_Count_Type;
      Depth     : in     Stack_Interfaces.Positive_Count_Type;
      Element : in     Element_Type);
   --  Set a Stack element. Index is the same as Peek Index; Depth is
   --  used to compute the index in the underlying array.
   --
   --  Stack must have been initialized by Set_Depth.
   --
   --  Useful when creating a stack from pre-existing data.

private

   subtype Positive_Count_Type is Ada.Containers.Count_Type range 1 .. Ada.Containers.Count_Type'Last;

   package Element_Arrays is new Ada.Containers.Vectors
     (Index_Type   => Positive_Count_Type,
      Element_Type => Element_Type);

   type Stack_Type is new Stack_Interfaces.Stack_Type with record
      Top  : Ada.Containers.Count_Type := 0; -- empty
      Data : Element_Arrays.Vector;
      --  Top of stack is at Data (Top).
   end record;

   Empty_Stack : constant Stack_Type := (0, Element_Arrays.Empty_Vector);

end SAL.Gen_Unbounded_Definite_Stacks;
