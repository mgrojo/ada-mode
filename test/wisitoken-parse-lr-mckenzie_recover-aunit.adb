--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2019 Stephen Leake All Rights Reserved.
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

pragma License (GPL);

with AUnit.Checks.Containers;
with SAL.AUnit;
with WisiToken.AUnit;
with WisiToken.LR.AUnit;
package body WisiToken.LR.McKenzie_Recover.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Token.Recover_Data'Class;
      Expected : in WisiToken.Token.Recover_Data'Class)
   is
      use Standard.AUnit.Checks.Containers;
      use Standard.AUnit.Checks;
      use SAL.AUnit;
      use WisiToken.AUnit;
      use WisiToken.LR.AUnit;

      Computed_Config : Configuration renames Configuration (Computed);
      Expected_Config : Configuration renames Configuration (Expected);
   begin
      Check (Label & ".Stack", Computed_Config.Stack, Expected_Config.Stack);
      Check (Label & ".Verb", Computed_Config.Verb, Expected_Config.Verb);
      Check
        (Label & ".Shared_Lookahead_Index",
         Computed_Config.Shared_Lookahead_Index,
         Expected_Config.Shared_Lookahead_Index);
      Check (Label & ".Local_Lookahead", Computed_Config.Local_Lookahead, Expected_Config.Local_Lookahead);
      Check
        (Label & ".Local_Lookahead_Index",
         Computed_Config.Local_Lookahead_Index,
         Expected_Config.Local_Lookahead_Index);
      Check (Label & ".Pushed", Computed_Config.Pushed, Expected_Config.Pushed);
      Check (Label & ".Popped", Computed_Config.Popped, Expected_Config.Popped);
      Check (Label & ".Inserted", Computed_Config.Inserted, Expected_Config.Inserted);
      Check (Label & ".Deleted", Computed_Config.Deleted, Expected_Config.Deleted);
      Check (Label & ".Cost", Computed_Config.Cost, Expected_Config.Cost);
   end Check;

end WisiToken.LR.McKenzie_Recover.AUnit;
