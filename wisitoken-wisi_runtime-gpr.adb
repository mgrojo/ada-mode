--  Abstract :
--
--  See spec
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

with Ada.Strings.Fixed;
package body WisiToken.Wisi_Runtime.Gpr is

   procedure Set_Params (Params : in String)
   is
      use Ada.Strings.Fixed;
      First : Integer := Params'First;
      Last  : Integer := Index (Params, " ");
   begin
      Gpr_Indent := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Gpr_Indent_Broken := Integer'Value (Params (First .. Last - 1));

      First := Last + 1;
      Last := Index (Params, " ", First);
      Gpr_Indent_When := Integer'Value (Params (First .. Last - 1));
   end Set_Params;

end WisiToken.Wisi_Runtime.Gpr;
