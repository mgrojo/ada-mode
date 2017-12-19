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

with AUnit.Checks.Text_IO;
with WisiToken.AUnit;
package body WisiToken.Token_Region.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Token;
      Expected : in Token)
   is
      use WisiToken.AUnit;
      use Standard.AUnit.Checks.Text_IO;
   begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check (Label & ".Line", Computed.Line, Expected.Line);
      Check (Label & ".Col", Computed.Col, Expected.Col);
      Check (Label & ".Char_Region", Computed.Char_Region, Expected.Char_Region);
      Check (Label & ".Byte_Region", Computed.Byte_Region, Expected.Byte_Region);
   end Check;

   procedure Check
     (Label              : in String;
      Computed           : in Error_Data;
      Expected           : in Error_Data;
      Check_Recover_Data : in WisiToken.Semantic_State.AUnit.Check_Recover_Type)
   is
      use WisiToken.AUnit;
      use WisiToken.Semantic_State.AUnit;
   begin
      Check (Label & ".Error_Token", Computed.Error_Token, Expected.Error_Token);
      Check (Label & ".Expecting", Computed.Expecting, Expected.Expecting);
      Check (Label & ".Recover", Computed.Recover, Expected.Recover, Check_Recover_Data);
   end Check;

end WisiToken.Token_Region.AUnit;
