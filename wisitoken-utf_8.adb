--  Abstract :
--
--  Utilities for UTF-8 encoded strings.
--
--  Copyright (C) 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);
with Interfaces;
package body WisiToken.UTF_8 is

   function Code_Point_Length (Item : in String) return Integer
   is begin
      return Result : Integer := 0 do
         for C of Item loop
            declare
               use Interfaces;
               Byte : constant Unsigned_8 := Unsigned_8 (Character'Pos (C));
            begin
               if ((Byte and 16#80#) = 16#80#) and ((Byte and 16#C0#) /= 16#C0#) then
                  --  https://en.wikipedia.org/wiki/UTF-8
                  --  byte 2, 3 or 4 of multi-byte UTF-8 char
                  null;
               else
                  Result := @ + 1;
               end if;
            end;
         end loop;
      end return;
   end Code_Point_Length;

end WisiToken.UTF_8;
