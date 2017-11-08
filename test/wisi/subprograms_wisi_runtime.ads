--  Abstract :
--
--  Ada implementation of language-specific action parameters used in subprograms.wy
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

package Subprograms_Wisi_Runtime is

   --  FIXME: pass these values to process from Emacs
   Subp_Indent        : Integer := 3;
   Subp_Indent_Broken : Integer := 2;

end Subprograms_Wisi_Runtime;
