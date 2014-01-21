--  Abstract :
--
--  Bug report from Georg Bauhaus 16 Jan 2014
--
--  Copyright (C) 2014  All Rights Reserved.
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
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.

pragma License (GPL);
package Georg_1 is
   package Typ is
      type Ltd is tagged limited null record;
      type Non_Tagged is limited null record;
   end Typ;

   generic
      type Formal is new Typ.Ltd with private; --!
   package F1 is end F1;

   generic
      type Formal is limited new Typ.Ltd with private; --!
   package F2 is end F2;

   generic
      type Formal is new Typ.Non_Tagged;  -- o.K.
   package F3 is end F3;

   generic
      type Formal is limited new Typ.Non_Tagged; --!
   package F4 is end F4;

   generic
      type Formal is abstract limited new Typ.Ltd with private; -- o.K.
   package F5 is end F5;

end Georg_1;
