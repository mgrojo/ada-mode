--  Abstract :
--
--  AUnit checks for parent
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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
with SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
with WisiToken.AUnit;
package WisiToken.LR.AUnit is

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Unknown_State_Index);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (All_Parse_Action_Verbs);

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

   package Token_Sequence_Arrays_AUnit is new Token_Sequence_Arrays.Gen_AUnit
     (Check_Index   => WisiToken.AUnit.Check,
      Check_Element => WisiToken.AUnit.Token_ID_Arrays_AUnit.Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Parse_Error_Label);

   procedure Check
     (Label    : in String;
      Computed : in Parse_Error;
      Expected : in Parse_Error);
   --  Does not check all fields

end WisiToken.LR.AUnit;
