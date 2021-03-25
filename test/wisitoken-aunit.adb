--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 - 2021 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

package body WisiToken.AUnit is

   procedure Check (Label : in String; Computed, Expected : in Production_ID)
   is begin
      Check (Label & ".nonterm", Computed.LHS, Expected.LHS);
      Standard.AUnit.Checks.Check (Label & ".rhs", Computed.RHS, Expected.RHS);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Buffer_Region;
      Expected : in WisiToken.Buffer_Region)
   is begin
      Check_Valid (Label & ".First valid", Computed.First'Unrestricted_Access);
      Check (Label & ".First", Computed.First, Expected.First);
      Check (Label & ".Last", Computed.Last, Expected.Last);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Line_Region;
      Expected : in WisiToken.Line_Region)
   is begin
      Check_Valid (Label & ".First valid", Computed.First'Unrestricted_Access);
      Check (Label & ".First", Computed.First, Expected.First);
      Check (Label & ".Last", Computed.Last, Expected.Last);
   end Check;

end WisiToken.AUnit;
