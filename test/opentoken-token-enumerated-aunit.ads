--  Abstract :
--
--  Support for AUnit tests involving parent.
--
--  Copyright (C) 2009 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

with AUnit.Check;
generic
package OpenToken.Token.Enumerated.AUnit is

   procedure Check is new Standard.AUnit.Check.Gen_Check_Discrete (Token_ID);

   type Token_Array is array (Natural range <>) of Token_ID;

   Null_Tokens : constant Token_Array := (1 .. 0 => Token_ID'First);

   procedure Check
     (Label    : in String;
      Computed : in OpenToken.Token.Linked_List.Instance;
      Expected : in Token_Array);

end OpenToken.Token.Enumerated.AUnit;
