--  Abstract :
--
--  AUnit Checks for parent
--
--  Copyright (C) 2017 - 2019 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Checks;
with SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
package WisiToken.AUnit is

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Unknown_State_Index);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Token_ID);

   procedure Check (Label : in String; Computed, Expected : in Production_ID);

   package Production_ID_Arrays_AUnit is new Production_ID_Arrays.Gen_AUnit
     (Check_Index   => Standard.AUnit.Checks.Check,
      Check_Element => Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Line_Number_Type);

   function To_Base_Token_Array (Item : in Token_ID_Array) return Base_Token_Arrays.Vector;

   procedure Check (Label : in String; Computed, Expected : in Base_Token);

   procedure Check (Label : in String; Computed, Expected : in Recover_Token);
   --  Not all components checked.

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Base_Token_Index);

   package Base_Token_Arrays_Aunit is new Base_Token_Arrays.Gen_AUnit
     (Check_Index   => Check,
      Check_Element => Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Unconstrained_Array
     (Item_Type   => Boolean,
      Index_Type  => Token_ID,
      Array_Type  => Token_ID_Set,
      Check_Index => WisiToken.AUnit.Check,
      Check_Item  => Standard.AUnit.Checks.Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Unconstrained_2D_Array
     (Item_Type     => Boolean,
      Index_1_Type  => Token_ID,
      Index_2_Type  => Token_ID,
      Array_Type    => Token_Array_Token_Set,
      Check_Index_1 => WisiToken.AUnit.Check,
      Check_Index_2 => WisiToken.AUnit.Check,
      Check_Item    => Standard.AUnit.Checks.Check);

   package Token_ID_Arrays_AUnit is new Token_ID_Arrays.Gen_AUnit
     (Check_Index   => Standard.AUnit.Checks.Check,
      Check_Element => Check);

   package Token_Sequence_Arrays_AUnit is new Token_Sequence_Arrays.Gen_AUnit
     (Check_Index   => Check,
      Check_Element => Token_ID_Arrays_AUnit.Check);

   procedure Check_Valid is new Standard.AUnit.Checks.Gen_Check_Valid (Buffer_Pos);
   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (Buffer_Pos);

   procedure Check
     (Label    : in String;
      Computed : in Buffer_Region;
      Expected : in Buffer_Region);

end WisiToken.AUnit;
