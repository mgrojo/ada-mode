--  Abstract :
--
--  See spec.
--
--  The parser deals only with token_ids; this package adds additional
--  information.
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

with WisiToken.AUnit;
package body WisiToken.Token_Region.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Token;
      Expected : in Token)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check (Label & ".Region", Computed.Region, Expected.Region);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Error_Data;
      Expected : in Error_Data)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".Error_Token", Computed.Error_Token, Expected.Error_Token);
      Check (Label & ".Expecting", Computed.Expecting, Expected.Expecting);
      Check (Label & ".Invalid_Region", Computed.Invalid_Region, Computed.Invalid_Region);
   end Check;

end WisiToken.Token_Region.AUnit;
