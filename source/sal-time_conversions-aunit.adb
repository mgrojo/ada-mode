--  Abstract :
--
--  See spec
--
--  Copyright (C) 2004 - 2006, 2009, 2015 Stephen Leake.  All Rights Reserved.
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

with SAL.AUnit.Assertions;
package body SAL.Time_Conversions.AUnit is

   procedure Check
     (Label     : in String;
      Computed  : in Time_Type;
      Expected  : in Time_Type;
      Tolerance : in Time_Type := Default_Tolerance)
   is begin
      Standard.SAL.AUnit.Assertions.Assert
        (abs (Computed - Expected) <= Tolerance,
         Label &
           " failed; expected " & Time_Type'Image (Expected) &
           " got " & Time_Type'Image (Computed));
   end Check;

end SAL.Time_Conversions.AUnit;
