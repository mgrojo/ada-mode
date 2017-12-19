--  Abstract :
--
--  AUnit Checks for parent
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with AUnit.Checks.Containers;
with SAL.AUnit;
package WisiToken.AUnit is

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (WisiToken.Token_ID);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (WisiToken.Line_Number_Type);

   type Plain_Token_ID_Array is array (Positive_Index_Type range <>) of Token_ID;

   function To_Base_Token_Array (Item : in Plain_Token_ID_Array) return Base_Token_Arrays.Vector;

   function To_Token_ID_Set (First, Last : in Token_ID; Item : in Plain_Token_ID_Array) return Token_ID_Set;
   --  First, Last determine size of result.
   --  For each element in Item, set result (element) True.

   procedure Check (Label : in String; Computed, Expected : in Base_Token);

   procedure Check is new Standard.AUnit.Checks.Containers.Gen_Check_Vector
     (Index_Type    => Positive_Index_Type,
      Element_Type  => Base_Token,
      Container_Pkg => Base_Token_Arrays,
      Check_Index   => SAL.AUnit.Check,
      Check_Element => Check);

   procedure Check
     is new Standard.AUnit.Checks.Gen_Check_Unconstrained_Array
       (Item_Type   => Boolean,
        Index_Type  => WisiToken.Token_ID,
        Array_Type  => WisiToken.Token_ID_Set,
        Check_Index => WisiToken.AUnit.Check,
        Check_Item  => Standard.AUnit.Checks.Check);

   procedure Check
     is new Standard.AUnit.Checks.Gen_Check_Unconstrained_2D_Array
       (Item_Type     => Boolean,
        Index_1_Type  => WisiToken.Token_ID,
        Index_2_Type  => WisiToken.Token_ID,
        Array_Type    => WisiToken.Token_Array_Token_Set,
        Check_Index_1 => WisiToken.AUnit.Check,
        Check_Index_2 => WisiToken.AUnit.Check,
        Check_Item    => Standard.AUnit.Checks.Check);

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Discrete (WisiToken.Buffer_Pos);

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Buffer_Region;
      Expected : in WisiToken.Buffer_Region);

end WisiToken.AUnit;
