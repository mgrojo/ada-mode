--  Abstract :
--
--  AUnit checks for parent
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL); -- AUnit is not Modified_GPL

with AUnit.Assertions;
with Ada.Tags;
package body WisiToken.Semantic_State.AUnit is

   procedure Check
     (Label              : in String;
      Computed           : in Recover_Data_Access;
      Expected           : in Recover_Data_Access;
      Check_Recover_Data : in Check_Recover_Type)
   is
      use all type Ada.Tags.Tag;
      use Standard.AUnit.Assertions;
   begin
      if Expected = null then
         if Check_Recover_Data = null then
            --  not checking Recover_Data
            null;
         else
            if Computed /= null then
               Assert (False, Label & ": expecting null");
            end if;
         end if;
      else
         if Computed = null then
            Assert (False, Label & ": got null");
         else
            if Check_Recover_Data = null then
               Assert (False, Label & "Check_Recover_Data = null");
            else
               Assert (Computed.all'Tag = Expected.all'Tag, "Recover_Data'Class mismatch");
               Check_Recover_Data (Label, Computed.all, Expected.all);
            end if;
         end if;
      end if;
   end Check;

end WisiToken.Semantic_State.AUnit;
