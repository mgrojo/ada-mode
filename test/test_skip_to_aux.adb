--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Text_IO;
with WisiToken.AUnit;
package body Test_Skip_To_Aux is

   procedure Test_Declaration_0 (Nonterm : in WisiToken.Base_Token)
   is
      use WisiToken.AUnit;
   begin
      if Enable then
         if WisiToken.Trace_Parse > WisiToken.Outline then
            Ada.Text_IO.Put_Line ("Test_Declaration_0");
         end if;

         Check ("declaration_0 1 byte region", Nonterm.Byte_Region, (11, 28)); -- DOS line endings
      end if;
   end Test_Declaration_0;

end Test_Skip_To_Aux;
