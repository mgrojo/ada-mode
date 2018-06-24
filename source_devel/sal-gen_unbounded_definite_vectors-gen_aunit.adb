--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake.  All Rights Reserved.
--
--  SAL is free software; you can redistribute it and/or modify it
--  under terms of the GNU General Public License as published by the
--  Free Software Foundation; either version 3, or (at your option)
--  any later version. SAL is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--  See the GNU General Public License for more details. You should
--  have received a copy of the GNU General Public License distributed
--  with SAL; see file COPYING. If not, write to the Free Software
--  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
--  USA.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with AUnit.Checks;
with SAL.AUnit;
package body SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Vector;
      Expected : in Vector)
   is
      use Standard.AUnit.Checks;
   begin
      if Computed = Empty_Vector then
         Check (Label & ".empty", Expected = Empty_Vector, True);
      else
         Check_Index (Label & ".First_Index", Computed.First_Index, Expected.First_Index);
         Check_Index (Label & ".Last_Index", Computed.Last_Index, Expected.Last_Index);
         for I in Computed.First_Index .. Computed.Last_Index loop
            Check_Element (Label & "." & Index_Type'Image (I), Computed (I), Expected (I));
         end loop;
      end if;
   end Check;

   procedure Check is new Standard.AUnit.Checks.Gen_Check_Access_Constant (Vector, Vector_Access);

   procedure Check
     (Label    : in String;
      Computed : in Cursor;
      Expected : in Cursor)
   is begin
      Check (Label & ".container", Computed.Container, Expected.Container);
      SAL.AUnit.Check (Label & ".index", Computed.Index, Expected.Index);
   end Check;

end SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
