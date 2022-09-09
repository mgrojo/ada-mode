--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Free Software Foundation, Inc.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Strings.Unbounded;
function SAL.Gen_Unbounded_Definite_Queues.Gen_Image_Aux (Item : in Queue; Aux : in Aux_Data) return String
is
   use Ada.Strings.Unbounded;
   use Element_Lists;
   Result : Unbounded_String        := To_Unbounded_String ("(");
   Last   : constant Base_Peek_Type := Item.Count;
   Cur    : Element_Lists.Cursor    := Item.Data.First;
begin
   for I in 1 .. Last loop
      Result := Result & Element_Image (Item.Data (Cur), Aux);
      if I /= Last then
         Result := Result & ", ";
      end if;
      Next (Cur);
   end loop;
   Result := Result & ")";
   return To_String (Result);
end SAL.Gen_Unbounded_Definite_Queues.Gen_Image_Aux;
