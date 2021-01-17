--  Abstract:
--
--  See spec.
--
--  Copyright (C) 2018 - 2021 Stephen Leake.  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Checks.Containers;
with SAL.AUnit;
package body SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Vector;
      Expected : in Vector)
   is
      use all type Ada.Containers.Count_Type;
      use Standard.AUnit.Checks.Containers;
      use Standard.AUnit.Checks;
   begin
      if Computed.Length = 0 then
         Check (Label & ".length", Computed.Length, Expected.Length);
      else
         Check_Index (Label & ".First_Index", Computed.First_Index, Expected.First_Index);
         Check_Index (Label & ".Last_Index", Computed.Last_Index, Expected.Last_Index);
         for I in Computed.First_Index .. Computed.Last_Index loop
            Check_Element (Label & "." & Index_Type'Image (I), Computed (I), Expected (I));
         end loop;
      end if;
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Cursor;
      Expected : in Cursor)
   is begin
      SAL.AUnit.Check (Label & ".index", Computed.Index, Expected.Index);
   end Check;

end SAL.Gen_Unbounded_Definite_Vectors.Gen_AUnit;
