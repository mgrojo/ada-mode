--  Abstract :
--
--  Utilities for WisiToken tests.
--
--  Copyright (C) 2021 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);
with GNAT.OS_Lib;
package WisiToken.Test_Util is

   procedure Spawn
     (Program     : in String;
      Args        : in GNAT.OS_Lib.String_List;
      Output_File : in String := "");

   procedure Dos2unix (File_Name : in String);

end WisiToken.Test_Util;
