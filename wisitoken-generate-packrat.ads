--  Abstract :
--
--  Types and operations for computing grammar properties used in
--  generating a packrat parser.
--
--  We use the terminology in [tratt 2010] for recursion in
--  productions.
--
--  References :
--
--  See wisitoken-parse-packrat.ads.
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

package WisiToken.Generate.Packrat is

   function Potential_Direct_Left_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set;
   --  Result (ID) is True if any production for ID can be direct left
   --  recursive; ie the first non-empty token is ID.
   --
   --  Empty must be from WisiToken.Generate.Has_Empty_Production.

   function Potential_Direct_Right_Recursive
     (Grammar : in WisiToken.Productions.Prod_Arrays.Vector;
      Empty   : in Token_ID_Set)
     return Token_ID_Set;
   --  Result (ID) is True if any production for ID can be direct right
   --  recursive; ie the last non-empty token is ID.
   --
   --  Empty must be from WisiToken.Generate.Has_Empty_Production.

end WisiToken.Generate.Packrat;
