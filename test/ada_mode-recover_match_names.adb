--  Abstract :
--
--  One each of all grammar constructs that can use the Match_Name check in error recovery.
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

package body Ada_Mode.Recover_Match_Names is

   procedure subprogram_body_0
   is begin
      loop
         --  Error here; typed 'loop;' instead of 'end loop;'
         --
         -- Without the Match_Name check, error recovery treats 'end
         -- subprogram_body' as matching 'loop'; the errors then cascade until
         -- a lot of new 'end's are needed at EOF.
         --
         -- With the Match_Name check, error recovery inserts the required 'end
         -- loop's here, so the errors do not cascade, and 'end
         -- subprogram_body' is indented properly.
         loop;

   end subprogram_body_0;

            --  FIXME: rest of named statements (search for END):
            --  accept_statement_0
            --  accept_statement_1
            --  block_statement_0
            --  block_statement_1
            --  entry_body_0
            --  loop_statement_0
            --  loop_statement_1
            --  ?
            --  package_body_0 without begin
            --  package_body_1 with begin
            --  package_specification_0 with private
            --  package_specification_1 without private
            --  ?

end Ada_Mode.Recover_Match_Names;
