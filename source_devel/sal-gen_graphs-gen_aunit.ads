--  Abstract :
--
--  AUnit checks for parent
--
--  Copyright (C) 2017, 2019 Stephen Leake All Rights Reserved.
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

with AUnit.Checks;
generic
   with procedure Check_Edge_Data (Label : in String; Computed, Expected : in Edge_Data);
package SAL.Gen_Graphs.Gen_AUnit is

   procedure Check is new AUnit.Checks.Gen_Check_Discrete (Vertex_Index);

   procedure Check
     (Label    : in String;
      Computed : in Path_Item;
      Expected : in Path_Item);

   procedure Check_Path is new AUnit.Checks.Gen_Check_Unconstrained_Array
     --  Has "Strict_Indices" param
     (Item_Type   => Path_Item,
      Index_Type  => Positive,
      Array_Type  => Path,
      Check_Index => AUnit.Checks.Check,
      Check_Item  => Check);

   procedure Check
     (Label    : in String;
      Computed : in Path;
      Expected : in Path);
   --  For composing.

end SAL.Gen_Graphs.Gen_AUnit;
