--  Abstract :
--
--  Report allocated memory use.
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

with GNATCOLL.Memory;

package Wisitoken.Memory_Statistics is

   function Dump_Memory_Statistics
     (Size   : Positive;
      Report : Report_Type := GNATCOLL.Memory.Memory_Usage)
      return String;
   --  Size is the number of the biggest memory users to show. Report
   --  indicates which sorting order is used in the report.

end Wisitoken.Memory_Statistics;
