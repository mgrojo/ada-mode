--  Abstract :
--
--  AUnit check for parent
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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
   with procedure Check_Element (Label : in String; Computed, Expected : in Element_Type);
package SAL.Gen_Definite_Doubly_Linked_Lists.Gen_AUnit is

   procedure Check (Label : in String; Computed : in List; Expected : in List);

   procedure Check (Label : in String; Computed : in Cursor; Expected : in Cursor);

end SAL.Gen_Definite_Doubly_Linked_Lists.Gen_AUnit;
