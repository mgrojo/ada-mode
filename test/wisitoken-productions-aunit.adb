--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

with AUnit.Checks;
package body WisiToken.Productions.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Instance;
      Expected : in Instance)
   is
      use Standard.AUnit.Checks;
      use WisiToken.AUnit;
      use Token_ID_Lists_AUnit;
   begin
      Check (Label & ".Name_Index", Computed.RHS.Name_Index, Expected.RHS.Name_Index);
      Check (Label & ".LHS", Computed.LHS, Expected.LHS);
      Check (Label & ".RHS", Computed.RHS.Tokens, Expected.RHS.Tokens);
   end Check;

end WisiToken.Productions.AUnit;
