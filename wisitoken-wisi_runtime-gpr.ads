--  Abstract :
--
--  Ada implementation of:
--
--  [1] gpr-wisi.el
--  [2] gpr-indent-user-options.el
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

package WisiToken.Wisi_Runtime.Gpr is

   --  Indent parameters from [2]
   Gpr_Indent        : Integer;
   Gpr_Indent_Broken : Integer;
   Gpr_Indent_When   : Integer;

   procedure Set_Params (Params : in String);
   --  Set all params from Params, in declaration order. Boolean is
   --  represented by 0 | 1. Parameter values are space delimited.

end WisiToken.Wisi_Runtime.Gpr;
