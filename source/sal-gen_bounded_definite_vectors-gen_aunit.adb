--  Abstract :
--
--  See spec.
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

package body SAL.Gen_Bounded_Definite_Vectors.Gen_AUnit is

   procedure Check (Label : in String; Computed, Expected : in Vector)
   is begin
      Check_Index (Label & "'Last", Computed.Last, Expected.Last);
      for I in Computed.Elements'First .. Base_Peek_Type (Computed.Last - Index_Type'First + 1) loop
         Check_Element (Label & "." & Base_Peek_Type'Image (I), Computed.Elements (I), Expected.Elements (I));
      end loop;
   end Check;

end SAL.Gen_Bounded_Definite_Vectors.Gen_AUnit;
