--  Abstract :
--
--  AUnit checks for parent
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL); -- AUnit is not Modified_GPL

with AUnit.Assertions;
with AUnit.Checks.Text_IO;
with WisiToken.AUnit;
package body WisiToken.Semantic_State.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Semantic_Action;
      Expected : in Semantic_Action)
   is begin
      Standard.AUnit.Assertions.Assert (Computed = Expected, Label & ": access type mismatch");
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Augmented_Token;
      Expected : in Augmented_Token)
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

end WisiToken.Semantic_State.AUnit;
