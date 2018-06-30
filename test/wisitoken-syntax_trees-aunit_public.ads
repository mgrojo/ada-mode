--  Abstract :
--
--  Public AUnit checks for parent
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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
with SAL.AUnit;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
package WisiToken.Syntax_Trees.AUnit_Public is

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Node_Index);

   package Valid_Node_Index_Arrays_AUnit is new Valid_Node_Index_Arrays.Gen_AUnit
     (Check_Index   => SAL.AUnit.Check,
      Check_Element => Check);

   procedure Check
     (Label    : in String;
      Computed : in Semantic_Action;
      Expected : in Semantic_Action);

end WisiToken.Syntax_Trees.AUnit_Public;
