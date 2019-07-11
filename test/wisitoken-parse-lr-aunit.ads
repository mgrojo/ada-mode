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

pragma License (GPL);

with AUnit.Checks;
with SAL.AUnit;
with SAL.Gen_Bounded_Definite_Vectors.Gen_AUnit;
package WisiToken.Parse.LR.AUnit is

   Strict : Boolean := False;

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (All_Parse_Action_Verbs);

   procedure Check
     (Label    : in String;
      Computed : in Minimal_Action;
      Expected : in Minimal_Action);
   --  If Expected.State is State_Index'Last, ignore it.

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

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Strategies);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Array
     (Item_Type   => Natural,
      Index_Type  => Strategies,
      Array_Type  => Strategy_Counts,
      Check_Index => Check,
      Check_Item  => Standard.AUnit.Checks.Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Parse_Error_Label);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Error;
      Expected : in Parse_Error);
   --  Does not check all fields

end WisiToken.Parse.LR.AUnit;
