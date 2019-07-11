--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017, 2018, 2019 Stephen Leake All Rights Reserved.
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

   function To_Base_Token_Array (Item : in Token_ID_Array) return Base_Token_Arrays.Vector
   is begin
      return
        Result : Base_Token_Arrays.Vector
      do
         for I of Item loop
            Result.Append ((I, others => <>));
         end loop;
      end return;
   end To_Base_Token_Array;

   procedure Check (Label : in String; Computed, Expected : in Base_Token)
   is begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check (Label & ".Byte_Region", Computed.Byte_Region, Expected.Byte_Region);
   end Check;

   procedure Check (Label : in String; Computed, Expected : in Recover_Token)
   is begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check (Label & ".Byte_Region", Computed.Byte_Region, Expected.Byte_Region);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Buffer_Region;
      Expected : in WisiToken.Buffer_Region)
   is begin
      Check_Valid (Label & ".First valid", Computed.First'Unrestricted_Access);
      Check_Valid (Label & ".Last valid", Computed.Last'Unrestricted_Access);
      Check (Label & ".First", Computed.First, Expected.First);
      Check (Label & ".Last", Computed.Last, Expected.Last);
   end Check;

end WisiToken.AUnit;
