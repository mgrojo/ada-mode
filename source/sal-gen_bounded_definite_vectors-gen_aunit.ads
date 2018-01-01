--  Abstract :
--
--  AUnit checks for instantiations of parent.
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

generic
   with procedure Check_Index (Label : in String; Computed, Expected : in Index_Type);
   with procedure Check_Element (Label : in String; Computed, Expected : in Element_Type);
package SAL.Gen_Bounded_Definite_Vectors.Gen_AUnit is

   procedure Check (Label : in String; Computed, Expected : in Vector);

end SAL.Gen_Bounded_Definite_Vectors.Gen_AUnit;
