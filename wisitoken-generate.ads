--  Abstract :
--
--  Types and operations for generating parsers, common to all parser
--  types.
--
--  The wisi* packages deal with reading *.wy files and generating
--  source code files. The wisitoken-generate* packages deal with
--  computing parser properties from the grammar. (For historical
--  reasons, not all packages follow this naming convention yet).
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

with WisiToken.Productions;
package WisiToken.Generate is

   function Has_Empty_Production
     (Grammar    : in WisiToken.Productions.Prod_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return Token_ID_Set;
   --  Result (ID) is True if any production for ID can be an empty
   --  production, recursively.

end WisiToken.Generate;
