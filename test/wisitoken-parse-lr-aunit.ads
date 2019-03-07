--  Abstract :
--
--  AUnit checks for parent
--
--  Copyright (C) 2017 - 2019 Stephen Leake All Rights Reserved.
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
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with AUnit.Checks;
with SAL.AUnit;
with SAL.Gen_Bounded_Definite_Vectors.Gen_AUnit;
with SAL.Gen_Definite_Doubly_Linked_Lists_Sorted.Gen_AUnit;
package WisiToken.Parse.LR.AUnit is

   Strict : Boolean := False;

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (All_Parse_Action_Verbs);

   procedure Check
     (Label    : in String;
      Computed : in Minimal_Action;
      Expected : in Minimal_Action);
   --  If Expected.State is State_Index'Last, ignore it.

   package Minimal_Action_Lists_AUnit is new Minimal_Action_Lists.Gen_AUnit (Check);

   procedure Check
     (Label    : in String;
      Computed : in Parse_State;
      Expected : in Parse_State);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Table;
      Expected : in Parse_Table);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Config_Op_Label);

   procedure Check
     (Label    : in String;
      Computed : in Config_Op;
      Expected : in Config_Op);

   procedure Check is new Config_Op_Arrays.Gen_AUnit (SAL.AUnit.Check, Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Parse_Error_Label);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Error;
      Expected : in Parse_Error);
   --  Does not check all fields

end WisiToken.Parse.LR.AUnit;
