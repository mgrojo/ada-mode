--  Abstract :
--
--  Utilities for AUnit tests of packages using Time_Conversions.
--
--  Copyright (C) 2004 - 2006, 2009 Stephen Leake.  All Rights Reserved.
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

package SAL.Time_Conversions.AUnit is
   pragma Elaborate_Body; --  parent.

   Default_Tolerance : Time_Type := 0.0;
   --  User may need to set this to some small number for their tests.

   procedure Check
     (Label     : in String;
      Computed  : in Time_Type;
      Expected  : in Time_Type;
      Tolerance : in Time_Type := Default_Tolerance);

end SAL.Time_Conversions.AUnit;
