--  Abstract :
--
--  see spec.
--
--  Copyright (C) 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with WisiToken.AUnit;
package body WisiToken.Lexer.AUnit is

   function To_Token_Array (Item : in Token_ID_Array) return Token_Arrays.Vector
   is begin
      return
        Result : Token_Arrays.Vector
      do
         for I of Item loop
            Result.Append ((I, others => <>));
         end loop;
      end return;
   end To_Token_Array;

   procedure Check (Label : in String; Computed, Expected : in Token)
   is
      use WisiToken.AUnit;
   begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check (Label & ".Byte_Region", Computed.Byte_Region, Expected.Byte_Region);
      Check (Label & ".Char_Region", Computed.Char_Region, Expected.Char_Region);
      Check (Label & ".Line_Region", Computed.Line_Region, Expected.Line_Region);
   end Check;

end WisiToken.Lexer.AUnit;
