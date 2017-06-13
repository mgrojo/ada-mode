--  Abstract :
--
--  Utilities for AUnit checks of types in Interfaces, Interfaces_More.
--  See SAL.AUnit for types in Standard.
--
--  Copyright (C) 2004 - 2006, 2009 - 2010, 2015 Stephen Leake.  All Rights Reserved.
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

with Interfaces.C;
with AUnit.Checks; use AUnit.Checks;
package SAL.Interfaces_More.AUnit is
   pragma Elaborate_Body; -- Standard.AUnit.Assertions

   procedure Check is new Gen_Check_Discrete (Unsigned_2);
   procedure Check is new Gen_Check_Discrete (Unsigned_3);
   procedure Check is new Gen_Check_Discrete (Unsigned_4);
   procedure Check is new Gen_Check_Discrete (Unsigned_5);
   procedure Check is new Gen_Check_Discrete (Unsigned_6);
   procedure Check is new Gen_Check_Discrete (Unsigned_7);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_8);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_10);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_12);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_14);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_16);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_17);
   procedure Check is new Gen_Check_Discrete (Interfaces_More.Unsigned_22);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_32);
   procedure Check is new Gen_Check_Discrete (Interfaces.Unsigned_64);

   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_8);
   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_16);
   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_32);
   procedure Check is new Gen_Check_Discrete (Interfaces.Integer_64);

   procedure Check_Binary
     (Label    : in String;
      Computed : in Interfaces.Unsigned_8;
      Expected : in Interfaces.Unsigned_8);
   --  Values in failure message are in binary

   procedure Check_Binary
     (Label    : in String;
      Computed : in Interfaces.Unsigned_16;
      Expected : in Interfaces.Unsigned_16);
   --  Values in failure message are in binary

   procedure Check_Hex
     (Label    : in String;
      Computed : in Interfaces.Unsigned_16;
      Expected : in Interfaces.Unsigned_16);
   --  Values in failure message are in decimal and hex

   procedure Check is new Gen_Check_Discrete (Interfaces.C.int);

   procedure Check
     (Label     : in String;
      Computed  : in Interfaces.Unsigned_16;
      Expected  : in Interfaces.Unsigned_16;
      Tolerance : in Interfaces.Unsigned_16);
   procedure Check
     (Label     : in String;
      Computed  : in Interfaces.Unsigned_32;
      Expected  : in Interfaces.Unsigned_32;
      Tolerance : in Interfaces.Unsigned_32);
   --  Sometimes we need a tolerance on these discrete types.

end SAL.Interfaces_More.AUnit;
