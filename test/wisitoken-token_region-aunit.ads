--  Abstract :
--
--  AUnit checks for parent
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

with WisiToken.Semantic_State.AUnit;
package WisiToken.Token_Region.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Token;
      Expected : in Token);

   procedure Check
     (Label              : in String;
      Computed           : in Error_Data;
      Expected           : in Error_Data;
      Check_Recover_Data : in WisiToken.Semantic_State.AUnit.Check_Recover_Type);

end WisiToken.Token_Region.AUnit;
