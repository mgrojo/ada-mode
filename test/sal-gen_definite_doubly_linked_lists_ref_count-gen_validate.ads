--  Abstract :
--
--  Check all link constraints in a list.
--
--  Copyright (C) 2017, 2018, 2021, 2022 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);

generic
package SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count.Gen_Validate is

   procedure Validate (Label : in String; Container : in List);

   procedure Check
     (Label    : in String;
      Computed : in Cursor;
      Expected : in Cursor);

   type Ref_Counts_Array is array (Positive range <>) of Integer;

   procedure Check_Ref_Counts
     (Label      : in String;
      Container  : in List;
      Ref_Counts : in Ref_Counts_Array);
   --  Check that Container element I has ref_count Ref_Counts (I).

end SAL.Gen_Definite_Doubly_Linked_Lists_Ref_Count.Gen_Validate;
