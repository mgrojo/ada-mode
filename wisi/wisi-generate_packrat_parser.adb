--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
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

procedure Wisi.Generate_Packrat_Parser
  (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
   Action_Names : in Names_Array_Array;
   Check_Names  : in Names_Array_Array)
is
   pragma Unreferenced (Grammar, Action_Names, Check_Names);
begin
   null;
end Wisi.Generate_Packrat_Parser;
