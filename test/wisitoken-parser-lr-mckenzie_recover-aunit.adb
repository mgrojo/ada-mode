--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 51 Franklin Street, Suite 500, Boston,
--  MA 02110-1335, USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with AUnit.Checks.Containers;
with WisiToken.AUnit;
with WisiToken.Parser.LR.AUnit;
package body WisiToken.Parser.LR.McKenzie_Recover.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Token.Recover_Data'Class;
      Expected : in WisiToken.Token.Recover_Data'Class)
   is
      use Standard.AUnit.Checks.Containers;
      use Standard.AUnit.Checks;
      use WisiToken.AUnit;
      use WisiToken.Parser.LR.AUnit;

      Computed_Config : Configuration renames Configuration (Computed);
      Expected_Config : Configuration renames Configuration (Expected);
   begin
      Check (Label & ".Stack", Computed_Config.Stack, Expected_Config.Stack);
      Check (Label & ".Lookahead_Index", Computed_Config.Lookahead_Index, Expected_Config.Lookahead_Index);
      Check (Label & ".Popped", Computed_Config.Popped, Expected_Config.Popped);
      Check (Label & ".Inserted", Computed_Config.Inserted, Expected_Config.Inserted);
      Check (Label & ".Deleted", Computed_Config.Deleted, Expected_Config.Deleted);
      Check (Label & ".Cost", Computed_Config.Cost, Expected_Config.Cost);
   end Check;

end WisiToken.Parser.LR.McKenzie_Recover.AUnit;
