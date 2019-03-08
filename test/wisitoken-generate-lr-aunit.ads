--  Abstract :
--
--  AUnit Checks for items in parent
--
--  Copyright (C) 2019 Stephen Leake All Rights Reserved.
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

with AUnit.Checks;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
with WisiToken.AUnit;
package WisiToken.Generate.LR.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in RHS_Sequence;
      Expected : in RHS_Sequence);

   package RHS_Sequence_Arrays_AUnit is new RHS_Sequence_Arrays.Gen_AUnit
     (Standard.AUnit.Checks.Check, Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Unconstrained_Array
     (Index_Type  => Token_ID,
      Item_Type   => RHS_Sequence_Arrays.Vector,
      Array_Type  => Minimal_Sequence_Array,
      Check_Index => WisiToken.AUnit.Check,
      Check_Item  => RHS_Sequence_Arrays_AUnit.Check);

end WisiToken.Generate.LR.AUnit;
