--  Abstract :
--
--  see spec.
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

package body SAL.Gen_Graphs.Gen_Aunit is

   procedure Check
     (Label    : in String;
      Computed : in Path_Item;
      Expected : in Path_Item)
   is begin
      Check (Label & ".Vertex", Computed.Vertex, Expected.Vertex);
      Check_Edge_Data (Label & ".Edge", Computed.Edge, Expected.Edge);
   end Check;

end SAL.Gen_Graphs.Gen_Aunit;
