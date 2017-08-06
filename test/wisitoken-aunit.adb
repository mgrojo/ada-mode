--  Abstract :
--
--  See spec
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.AUnit is

   function To_Token_Array (Item : in Plain_Token_Array) return Token_Arrays.Vector
   is begin
      return
        Result : Token_Arrays.Vector
      do
         Result.Reserve_Capacity (Item'Length);
         for I of Item loop
            Result.Append (I);
         end loop;
      end return;
   end To_Token_Array;

   procedure Check
     (Label    : in String;
      Computed : in WisiToken.Buffer_Region;
      Expected : in WisiToken.Buffer_Region)
   is
      use Standard.AUnit.Checks;
   begin
      Check (Label & ".Begin_Pos", Computed.Begin_Pos, Expected.Begin_Pos);
      Check (Label & ".End_Pos", Computed.End_Pos, Expected.End_Pos);
   end Check;

end WisiToken.AUnit;
