--  Abstract :
--
--  Utilities for UTF-8 encoded strings not provided by
--  Ada.Strings.UTF_Encoding*.
--
--  Copyright (C) 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);
package WisiToken.UTF_8 is

   function Code_Point_Length (Item : in String) return Integer;

end WisiToken.UTF_8;
