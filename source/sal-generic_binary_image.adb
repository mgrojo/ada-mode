--  Abstract:
--
--  see spec
--
--  Copyright (C) 2005, 2009, 2010, 2012 Stephen Leake.  All Rights Reserved.
--
--  This library is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This library is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this  unit  does not  by itself cause  the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file  might be covered by the  GNU Public License.

pragma License (Modified_GPL);

function SAL.Generic_Binary_Image (Item : in Number_Type) return String
is
   Temp : Number_Type := Item;
   Nibble : Number_Type;
   Image : String (1 .. Nibbles * 4 + Nibbles - 1);
begin
   for I in reverse Image'Range loop
      if I mod 5 = 0 then
         Image (I) := '_';
      else
         Nibble := Temp mod 2;
         Temp := Temp / 2;
         if Nibble = 0 then
            Image (I) := '0';
         else
            Image (I) := '1';
         end if;
      end if;
   end loop;
   return Image;
end SAL.Generic_Binary_Image;
