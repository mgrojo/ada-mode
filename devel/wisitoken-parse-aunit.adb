--  Abstract :
--
--  see spec.
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

with WisiToken.AUnit;
package body WisiToken.Parse.AUnit is

   procedure Check
     (Label : in String;
      Computed : in KMN;
      Expected : in KMN)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".Stable_Bytes", Computed.Stable_Bytes, Expected.Stable_Bytes);
      Check (Label & ".Stable_Chars", Computed.Stable_Chars, Expected.Stable_Chars);
      Check (Label & ".Inserted_Bytes", Computed.Inserted_Bytes, Expected.Inserted_Bytes);
      Check (Label & ".Inserted_Chars", Computed.Inserted_Chars, Expected.Inserted_Chars);
      Check (Label & ".Deleted_Bytes", Computed.Deleted_Bytes, Expected.Deleted_Bytes);
      Check (Label & ".Deleted_Chars", Computed.Deleted_Chars, Expected.Deleted_Chars);
   end Check;

end WisiToken.Parse.AUnit;
