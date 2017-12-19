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

pragma License (GPL);

with AUnit.Checks;
with WisiToken.AUnit;
package body WisiToken.Token_ID_Lists.AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Cursor;
      Expected : in Cursor)
   is
      use Standard.AUnit.Checks;
      use WisiToken.AUnit;
      Computed_I : Cursor  := Computed;
      Expected_I : Cursor  := Expected;
      Index      : Integer := 1;
   begin
      loop
         if Computed_I = No_Element or Expected_I = No_Element then
            Check (Label & " = null", Computed_I = No_Element and Expected_I = No_Element, True);
            exit;
         end if;
         Check (Label & Integer'Image (Index), Element (Computed_I), Element (Expected_I));
         Next (Computed_I);
         Next (Expected_I);
         Index := Index + 1;
      end loop;
   end Check;

end WisiToken.Token_ID_Lists.AUnit;
